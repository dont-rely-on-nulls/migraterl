%%-------------------------------------------------------------------
% @doc Library module.
% @end
%%-------------------------------------------------------------------
-module(migraterl).

-export([init/1, migrate/2]).
-export([default_connection/0]).

-include("internal_types.hrl").

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "postgres")).
-define(PGDATABASE, os:getenv("PGDATABASE", "migraterl")).

%% @doc A default connection, for local testing or CI.
%% @end
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

%% @doc Applies a migration file to the Database.
%% @end
-spec upgrade(Conn :: epgsql:connection(), Filename :: filename()) -> Result when
    Result :: ok | error().
upgrade(Conn, Filename) ->
    {ok, Bin} = file:read_file(Filename),
    SQL = file_utils:format_bin_content(Bin),
    case epgsql:squery(Conn, SQL) of
        {ok, _, _} -> ok;
        Otherwise -> {error, upgrade_failure, Otherwise}
    end.

%% @doc Applies a migration file to the Database.
%% @end
-spec upgrade(Conn :: epgsql:connection(), Version :: integer(), Dir :: directory()) -> Result when
    Result :: ok | {error, any()}.
upgrade(Conn, _Version, Dir) ->
    {ok, Files} = file_utils:read_directory(Dir),
    % TODO: Rewrite this to be in a single transaction
    _X = lists:map(fun(F) -> upgrade(Conn, F) end, Files),
    ok.

%% @doc Creates the required migraterl tables on the Database.
%% @end
-spec init(Conn :: epgsql:connection()) -> Result when
    Result :: ok | error().
init(Conn) ->
    {ok, Dir} = file_utils:read_system_migrations(?MODULE),
    upgrade(Conn, Dir).

%% @doc
%% Given a directory, applies only the files not already present on
%% the migration. If the migration table does not yet exist, make sure
%% to create it beforehand.
%% @end
-spec migrate(Conn :: epgsql:connection(), Dir :: directory()) -> Result when
    Result :: ok | error().
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
