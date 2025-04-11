%%-------------------------------------------------------------------
% @doc Library module.
% @end
%%-------------------------------------------------------------------
-module(migraterl).

-export([init/1, migrate/2]).
-export([default_connection/0]).

-include("file_utils.hrl").
-include("migraterl.hrl").

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "migraterl")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "postgres")).
-define(PGDATABASE, os:getenv("PGDATABASE", "migraterl")).

-dialyzer({nowarn_function, [init/1]}).

%% @doc A default connection, for local testing or CI.
%% @end
-spec default_connection() -> Result when
    Error :: {error, db_connection_error, Message :: string()},
    Result :: epgsql:connection() | Error.
default_connection() ->
    Config =
        #{
            host => ?PGHOST,
            port => ?PGPORT,
            username => ?PGUSER,
            password => ?PGPASSWORD,
            database => ?PGDATABASE,
            timeout => 4000
        },
    case epgsql:connect(Config) of
        {ok, Conn} ->
            Conn;
        Otherwise ->
            Message = io_lib:format("Error while setting connection ~p~n", Otherwise),
            {error, db_connection_error, Message}
    end.

%% @doc Applies a migration file to the Database.
%% @end
-spec upgrade(Conn, Filename) -> Result when
    Conn :: epgsql:connection(),
    Filename :: filename(),
    Error :: {error, upgrade_failure, Reason :: string()},
    Result :: ok | Error.
upgrade(Conn, Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, SQL} = file_utils:format_bin_content(Bin),
    case epgsql:squery(Conn, SQL) of
        {ok, _, _} -> ok;
        Otherwise -> {error, upgrade_failure, Otherwise}
    end.

%% @doc Applies a migration file to the Database.
%% @todo Rewrite this to run everything in a single transaction.
%% @end
-spec upgrade(Conn, Version, Dir) -> Result when
    Conn :: epgsql:connection(),
    Version :: version(),
    Dir :: directory(),
    Error :: {error, any(), any()},
    Result :: ok | Error.
upgrade(Conn, Version, Dir) ->
    {ok, Files} = file_utils:read_directory(Dir),
    Migrations = lists:nthtail(Version, Files),
    _X = lists:map(fun(F) -> upgrade(Conn, F) end, Migrations),
    ok.

%% @doc Creates the required migraterl tables on the Database.
%% @end
-spec init(Conn :: epgsql:connection()) -> Result when
    Error :: {error, any(), any()},
    Result :: ok | Error.
init(Conn) ->
    {ok, Dir} = file_utils:read_system_migrations(),
    upgrade(Conn, 0, Dir).

%% @doc
%% Given a directory, applies only the files not already present on
%% the migration. If the migration table does not yet exist, make sure
%% to create it beforehand.
%% @end
-spec migrate(Conn :: epgsql:connection(), Dir :: directory()) -> Result when
    Error :: {error, any(), any()},
    Result :: ok | Error.
migrate(Conn, Dir) ->
    {ok, Files} = file_utils:read_directory(Dir),
    Query = """
        SELECT COALESCE(MAX(version),0) as last_version
        FROM migraterl.history
    """,
    case epqsql:squery(Conn, Query) of
        {error, _} ->
            ok = init(Conn),
            upgrade(Conn, 0, Files);
        {ok, _, [{Version}]} ->
            upgrade(Conn, binary_to_integer(Version), Files);
        Otherwise ->
            Message = io_lib:format("Unmapped Case: ~p~n", Otherwise),
            logger:error(Message),
            {error, unmapped_case, Message}
    end.
