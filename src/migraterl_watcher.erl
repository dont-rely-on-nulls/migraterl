-module(migraterl_watcher).
-moduledoc """
Long-lived, supervised directory watcher (development workflow).

Modelled as a `gen_statem` with two states:

- `idle`: steady state, waiting for filesystem activity.
- `debouncing`: a relevant change arrived; a `state_timeout` coalesces
  a burst of events (editor write-then-rename, formatters, git
  checkouts) into a single migration run.

One watcher is started per entry in the `watchers` application
environment (see `m:migraterl_sup`). It uses the optional `fs`
application to observe the source directories of a run configuration
and re-runs `migraterl:migrate/2` after the debounce window.
For lifecycle layouts it observes the layout root so optional stage
directories created later are covered too.

Meant for local development ("apply on save"); production deploys
should call `migraterl:migrate/2` explicitly at boot.

A watcher spec is a map:

```erlang
#{conn => epgsql:connection() | epgsql:connect_opts(),
  migrate => map(),            % the migraterl:migrate/2 options map
  debounce_ms => non_neg_integer()}
```
""".
-behaviour(gen_statem).

-include("migraterl.hrl").

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3, debouncing/3]).

-define(DEFAULT_DEBOUNCE_MS, 300).

-record(data, {
    conn :: epgsql:connection(),
    owns_conn :: boolean(),
    migrate :: map(),
    debounce :: non_neg_integer(),
    dirs :: [binary()]
}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Spec) ->
    gen_statem:start_link(?MODULE, Spec, []).

callback_mode() -> state_functions.

init(Spec) ->
    process_flag(trap_exit, true),
    Migrate = maps:get(migrate, Spec),
    case migraterl_config:normalize(Migrate) of
        {ok, Opts} -> init_validated(Spec, Migrate, Opts);
        {error, Reason} -> {stop, {invalid_migrate_options, Reason}}
    end.

init_validated(Spec, Migrate, Opts) ->
    Dirs = [normalize(Dir) || Dir <- migraterl_config:watch_dirs(Opts)],
    case validate_watch_dirs(Dirs) of
        ok ->
            case ensure_fs() of
                ok -> init_fs(Spec, Migrate, Dirs);
                {error, Reason} -> {stop, {fs_unavailable, Reason}}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

init_fs(Spec, Migrate, Dirs) ->
    case start_all_fs(Dirs) of
        ok ->
            init_connection(Spec, Migrate, Dirs);
        {error, Reason} ->
            {stop, Reason}
    end.

init_connection(Spec, Migrate, Dirs) ->
    ConnSpec = maps:get(conn, Spec),
    case connect(ConnSpec) of
        {ok, Conn} ->
            Debounce = maps:get(debounce_ms, Spec, ?DEFAULT_DEBOUNCE_MS),
            Data = #data{
                conn = Conn,
                owns_conn = is_map(ConnSpec),
                migrate = Migrate,
                debounce = Debounce,
                dirs = Dirs
            },
            {ok, idle, Data};
        {error, Reason} ->
            {stop, {connect_failed, Reason}}
    end.

%% idle: a relevant change starts the debounce window.
idle(info, {_Pid, {fs, file_event}, {Path, _Flags}}, Data) ->
    case relevant(Path, Data) of
        true -> {next_state, debouncing, Data, [{state_timeout, Data#data.debounce, run}]};
        false -> keep_state_and_data
    end;
idle(info, {'EXIT', Conn, Reason}, #data{conn = Conn}) ->
    {stop, {connection_down, Reason}};
idle(_Type, _Event, _Data) ->
    keep_state_and_data.

%% debouncing: further changes restart the timer; on expiry we migrate.
debouncing(info, {_Pid, {fs, file_event}, {Path, _Flags}}, Data) ->
    case relevant(Path, Data) of
        %% setting a new state_timeout cancels the pending one
        true -> {keep_state_and_data, [{state_timeout, Data#data.debounce, run}]};
        false -> keep_state_and_data
    end;
debouncing(state_timeout, run, Data) ->
    _ = run(Data),
    {next_state, idle, Data};
debouncing(info, {'EXIT', Conn, Reason}, #data{conn = Conn}) ->
    {stop, {connection_down, Reason}};
debouncing(_Type, _Event, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, #data{owns_conn = true, conn = Conn}) ->
    _ = epgsql:close(Conn),
    ok;
terminate(_Reason, _State, _Data) ->
    ok.

%% ---------------
%% Helpers
%% ---------------

run(#data{conn = Conn, migrate = Migrate}) ->
    Result = migraterl:migrate(Conn, Migrate),
    logger:info("migraterl_watcher: auto-run result ~p", [Result]),
    Result.

connect(Conn) when is_pid(Conn) -> {ok, Conn};
connect(ConnConfig) when is_map(ConnConfig) -> epgsql:connect(ConnConfig).

ensure_fs() ->
    case application:ensure_all_started(fs) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

validate_watch_dirs([]) ->
    ok;
validate_watch_dirs([Dir | Rest]) ->
    case filelib:is_dir(Dir) of
        true -> validate_watch_dirs(Rest);
        false -> {error, {watch_failed, Dir, enoent}}
    end.

start_all_fs([]) ->
    ok;
start_all_fs([Dir | Rest]) ->
    case start_fs(Dir) of
        {ok, Name} ->
            case fs:subscribe(Name) of
                ok -> start_all_fs(Rest);
                {error, Reason} -> {error, {watch_subscribe_failed, Dir, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

start_fs(Dir) ->
    Name = binary_to_atom(<<"migraterl_fs_", Dir/binary>>, utf8),
    case fs:start_link(Name, Dir) of
        {ok, _} -> {ok, Name};
        {error, {already_started, _}} -> {ok, Name};
        {error, Reason} -> {error, {watch_failed, Dir, Reason}};
        Other -> {error, {watch_failed, Dir, Other}}
    end.

normalize(Dir) ->
    iolist_to_binary(filename:absname(Dir)).

relevant(Path, #data{dirs = Dirs}) ->
    Bin = iolist_to_binary(filename:absname(Path)),
    is_sql(Bin) andalso lists:any(fun(Dir) -> prefix(Dir, Bin) end, Dirs).

is_sql(Path) ->
    filename:extension(Path) =:= <<".sql">>.

prefix(Dir, Path) ->
    binary:match(Path, Dir) =:= {0, byte_size(Dir)}.
