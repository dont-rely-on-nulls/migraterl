-module(migraterl).

-export([init/1, migrate/2]).
-export([default_connection/0]).

-include("internal_types.hrl").

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "postgres")).
-define(PGDATABASE, os:getenv("PGDATABASE", "migraterl")).

-spec default_connection() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
default_connection() ->
    Connection =
        #{
            host => ?PGHOST,
            port => ?PGPORT,
            username => ?PGUSER,
            password => ?PGPASSWORD,
            database => ?PGDATABASE,
            timeout => 4000
        },
    epgsql:connect(Connection).

-spec init(Conn :: epgsql:connection()) -> ok.
init(Conn) ->
    {ok, Files} = file_utils:read_system_migrations(?MODULE),
    % TODO: Rewrite this to be in a single transaction
    _X = lists:map(fun(F) -> upgrade(Conn, F) end, Files),
    ok.

-spec upgrade(Conn :: epgsql:connection(), Filename :: filename()) -> ok | {error, any()}.
upgrade(Conn, Filename) ->
    {ok, Bin} = file:read_file(Filename),
    SQL = file_utils:format_bin_content(Bin),
    case epgsql:squery(Conn, SQL) of
        {ok, _, _} -> ok;
        Otherwise -> {error, Otherwise}
    end.

-spec upgrade(Conn :: epgsql:connection(), Version :: integer(), File :: directory()) -> ok | {error, any()}.
upgrade(Conn, _Version, File) ->
    {ok, Files} = file_utils:read_directory(File),
    % TODO: Rewrite this to be in a single transaction
    _X = lists:map(fun(F) -> upgrade(Conn, F) end, Files),
    ok.

-spec migrate(Conn :: epgsql:connection(), Dir :: directory()) -> ok | {error, any()}.
migrate(Conn, Dir) ->
    {ok, Files} = file_utils:read_directory(Dir),
    Version = 1,
    case epqsql:squery(Conn, "SELECT MAX(version) FROM migraterl_history") of
        {error, _Error} ->
            ok = init(Conn),
            upgrade(Conn, Version, Files);
        {ok, _} ->
            upgrade(Conn, Version, Files)
    end.
