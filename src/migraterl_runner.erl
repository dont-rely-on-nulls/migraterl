-module(migraterl_runner).
-moduledoc """
The migration runner, modelled as a `gen_statem`.

A run is a short-lived, linear lifecycle where each phase is an
explicit state and any failure routes to `failed`. The caller starts a
runner and blocks on a single `run` call that is answered only once a
terminal state is reached:

```text
idle -> ensuring_journal -> locking -> reading_state
     -> scanning -> planning -> applying -> done
                                   |          |
                                (dry_run)  failed
```

The namespace advisory lock is acquired in `locking` and released
before the caller is answered (with `terminate/3` as a crash-path
safety net), so it is held for the whole run regardless of the
transaction mode. Transaction boundaries themselves are emitted from
the `applying` loop, which lets one code path serve per_script, single
and none modes.
""".
-behaviour(gen_statem).

-include("migraterl.hrl").

-export([run/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([
    idle/3,
    ensuring_journal/3,
    locking/3,
    reading_state/3,
    scanning/3,
    planning/3,
    applying/3
]).

-record(data, {
    conn :: epgsql:connection(),
    opts :: #opts{},
    from :: gen_statem:from() | undefined,
    locked = false :: boolean(),
    journal = #{} :: #{binary() => #entry{}},
    scripts = [] :: [#script{}],
    plan :: #plan{} | undefined,
    %% action names in the plan, in application order (set at planning)
    planned = [] :: [binary()],
    %% names applied so far, newest last
    applied = [] :: [binary()]
}).

-doc """
Run a migration to completion and return its result. Blocks the caller
until the runner reaches a terminal state.
""".
-spec run(epgsql:connection(), #opts{}, timeout()) ->
    {ok, #{applied := [binary()], skipped := [binary()], warnings := [warning()]}}
    | {error, term()}.
run(Conn, Opts, Timeout) ->
    case gen_statem:start(?MODULE, {Conn, Opts}, []) of
        {ok, Pid} ->
            try
                gen_statem:call(Pid, run, Timeout)
            catch
                exit:Reason -> {error, {runner_exit, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

callback_mode() -> state_functions.

init({Conn, Opts}) ->
    {ok, idle, #data{conn = Conn, opts = Opts}}.

%% idle: wait for the caller's run request, remember whom to answer, and
%% kick the pipeline off with an internal event.
idle({call, From}, run, Data) ->
    {next_state, ensuring_journal, Data#data{from = From}, [{next_event, internal, proceed}]}.

ensuring_journal(internal, proceed, #data{conn = Conn} = Data) ->
    step(
        fun() ->
            case migraterl_pg:assert_version(Conn) of
                ok -> migraterl_pg:ensure_journal(Conn);
                Err -> Err
            end
        end,
        locking,
        Data
    ).

locking(internal, proceed, #data{conn = Conn, opts = Opts} = Data) ->
    case migraterl_pg:advisory_lock(Conn, Opts#opts.namespace) of
        ok ->
            advance(reading_state, Data#data{locked = true});
        {error, _} = Err ->
            fail(Err, Data)
    end.

reading_state(internal, proceed, #data{conn = Conn, opts = Opts} = Data) ->
    case migraterl_pg:current_entries(Conn, Opts#opts.namespace) of
        {ok, Journal} -> advance(scanning, Data#data{journal = Journal});
        {error, _} = Err -> fail(Err, Data)
    end.

scanning(internal, proceed, #data{opts = Opts} = Data) ->
    case migraterl_scanner:scan(Opts#opts.namespace, Opts#opts.sources) of
        {ok, Scripts} -> advance(planning, Data#data{scripts = Scripts});
        {error, _} = Err -> fail(Err, Data)
    end.

planning(internal, proceed, #data{scripts = Scripts, journal = Journal, opts = Opts} = Data) ->
    Plan = migraterl_plan:build(Scripts, Journal, Opts),
    Planned = [(A#action.script)#script.name || A <- Plan#plan.actions],
    Data1 = Data#data{plan = Plan, planned = Planned},
    case fatal_warnings(Plan, Opts) of
        [] when Opts#opts.dry_run ->
            done(Data1);
        [] ->
            advance(applying, Data1);
        Fatal ->
            fail({error, {out_of_order, Fatal}}, Data1)
    end.

%% applying: emit the transaction prologue (single mode only), then walk the
%% plan one action at a time via internal `apply_next' events, then the epilogue.
applying(internal, proceed, #data{conn = Conn, opts = Opts, plan = Plan} = Data) ->
    case Plan#plan.actions of
        [] ->
            done(Data);
        _ ->
            case enter_apply(Conn, Opts) of
                ok -> {keep_state, Data, [{next_event, internal, apply_next}]};
                {error, _} = Err -> fail(Err, Data)
            end
    end;
applying(internal, apply_next, #data{plan = #plan{actions = []}, conn = Conn, opts = Opts} = Data) ->
    case finish_apply(Conn, Opts) of
        ok -> done(Data);
        {error, _} = Err -> fail(Err, Data)
    end;
applying(
    internal,
    apply_next,
    #data{plan = #plan{actions = [Action | Rest]} = Plan, conn = Conn, opts = Opts} = Data
) ->
    #action{script = #script{name = Name}} = Action,
    case apply_action(Conn, Action, Opts) of
        {ok, _Ms} ->
            Data1 = Data#data{
                plan = Plan#plan{actions = Rest},
                applied = [Name | Data#data.applied]
            },
            {keep_state, Data1, [{next_event, internal, apply_next}]};
        {error, Reason} ->
            _ = abort(Conn, Opts),
            fail({error, {apply_failed, Name, Reason}}, Data)
    end.

%% ---------------
%% Transaction boundary emission per mode
%% ---------------
%%
%% single     : one BEGIN before the loop, one COMMIT after; each action runs
%%              bare inside it.
%% per_script : each action is wrapped in its own BEGIN/COMMIT.
%% none       : no transaction control at all.

enter_apply(Conn, #opts{txn = single}) -> migraterl_pg:begin_txn(Conn);
enter_apply(_Conn, _Opts) -> ok.

finish_apply(Conn, #opts{txn = single}) -> migraterl_pg:commit(Conn);
finish_apply(_Conn, _Opts) -> ok.

apply_action(Conn, Action, #opts{txn = per_script} = Opts) ->
    case migraterl_pg:begin_txn(Conn) of
        ok ->
            case migraterl_pg:apply_one(Conn, Action, Opts) of
                {ok, _} = Ok ->
                    case migraterl_pg:commit(Conn) of
                        ok -> Ok;
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
apply_action(Conn, Action, Opts) ->
    migraterl_pg:apply_one(Conn, Action, Opts).

%% Roll back the in-flight transaction (the current action's for per_script,
%% the whole run's for single). No-op when running without transactions.
abort(_Conn, #opts{txn = none}) -> ok;
abort(Conn, _Opts) -> migraterl_pg:rollback(Conn).

%% ---------------
%% Terminal helpers
%% ---------------

%% Run a side-effecting step, advancing on ok and failing otherwise.
step(Fun, Next, Data) ->
    case Fun() of
        ok -> advance(Next, Data);
        {error, _} = Err -> fail(Err, Data)
    end.

advance(Next, Data) ->
    {next_state, Next, Data, [{next_event, internal, proceed}]}.

done(#data{plan = Plan, planned = Planned, applied = Applied} = Data0) ->
    #data{from = From} = Data = release(Data0),
    Summary = #{
        planned => Planned,
        applied => lists:reverse(Applied),
        skipped => skipped(Plan),
        warnings => warnings(Plan)
    },
    {stop_and_reply, normal, [{reply, From, {ok, Summary}}], Data}.

fail({error, Reason}, Data0) ->
    #data{from = From} = Data = release(Data0),
    {stop_and_reply, normal, [{reply, From, {error, Reason}}], Data}.

%% Release the advisory lock before replying, so the caller can safely reuse
%% the connection the instant it receives the result (terminate/3 would
%% otherwise race the caller for the shared epgsql connection).
release(#data{locked = true, conn = Conn, opts = Opts} = Data) ->
    ok = migraterl_pg:advisory_unlock(Conn, Opts#opts.namespace),
    Data#data{locked = false};
release(Data) ->
    Data.

skipped(undefined) -> [];
skipped(#plan{skipped = S}) -> S.

warnings(undefined) -> [];
warnings(#plan{warnings = W}) -> W.

fatal_warnings(#plan{warnings = Warnings}, #opts{on_out_of_order = error}) ->
    [Name || {out_of_order, Name} <- Warnings];
fatal_warnings(_Plan, _Opts) ->
    [].

terminate(_Reason, _State, #data{locked = true, conn = Conn, opts = Opts}) ->
    migraterl_pg:advisory_unlock(Conn, Opts#opts.namespace);
terminate(_Reason, _State, _Data) ->
    ok.
