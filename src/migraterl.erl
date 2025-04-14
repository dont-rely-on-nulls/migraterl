%%-------------------------------------------------------------------
% @doc Library module.
% @end
%%-------------------------------------------------------------------
-module(migraterl).

-export([init/1, migrate/2]).
-export([default_connection/0]).

-include("file_utils.hrl").
-include("migraterl.hrl").

-define(PGHOST, os:getenv("PG_HOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "migraterl")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "migraterl")).
-define(PGDATABASE, os:getenv("PGDATABASE", "migraterl")).

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
            timeout => 10000
        },
    case epgsql:connect(Config) of
        {ok, Conn} ->
            Conn;
        Otherwise ->
            Message = io_lib:format("Error while setting connection ~p~n", [Otherwise]),
            {error, db_connection_error, Message}
    end.

%% @doc
%% Collects all SQL statements from a list for Files
%% @end
aggregate(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, SQL} = file_utils:format_bin_content(Bin),
    SQL.

%% @doc
%% Check wether the output of epgsql
%% @end
proper([]) ->
    true;
proper([H | T]) ->
    case H of
        {ok, _} -> proper(T);
        {ok, _, _} -> proper(T);
        _ -> false
    end.

%% @doc
%% Applies a list of SQLStatements, either works or sends a rollback (with a reason) to eqpgsql.
%% @end
-spec run(Conn, SQLStatements) -> Result when
    Conn :: epgsql:connection(),
    SQLStatements :: [string()],
    Reason :: string(),
    Error :: {rollback, Reason},
    Result :: ok | Error.
run(_, []) ->
    ok;
run(Conn, [Query | Rest]) ->
    Out = epgsql:squery(Conn, Query),
    case Out of
        {ok, _, _} ->
            run(Conn, Rest);
        {ok, _} ->
            run(Conn, Rest);
        C when is_list(C) ->
            case proper(C) of
                true ->
                    run(Conn, Rest);
                false ->
                    logger:error("[RUN] LIST: ~p~n", [C]),
                    {rollback, "Bad match while parsing list"}
            end;
        {error, Reason} ->
            logger:error("[RUN] QUERY: ~p~n REST: ~p~n", [Query, Rest]),
            {rollback, Reason}
    end.

%% @doc Applies a migration file to the Database.
%% @todo Rewrite this to run everything in a single transaction.
%% @end
-spec upgrade(Conn, Version, Dir) -> Result when
    Conn :: epgsql:connection(),
    Version :: version(),
    Dir :: directory(),
    Reason :: string(),
    Other :: term(),
    Ok :: {ok, Other},
    Error :: {error, Reason},
    Result :: Ok | Error.
upgrade(Conn, Version, Dir) ->
    {ok, Files} = file_utils:read_directory(Dir),
    Migrations = lists:nthtail(Version, Files),
    Statements = lists:map(fun(F) -> aggregate(F) end, Migrations),
    Fun = fun(_) -> run(Conn, Statements) end,
    case epgsql:with_transaction(Conn, Fun) of
        {rollback, Reason} ->
            logger:error("[UPGRADE] ~p~n", [Reason]),
            {error, Reason};
        _Other ->
            ok
    end.

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
    Query = "SELECT COALESCE(MAX(version),0) as last_version FROM migraterl.history",
    case epgsql:squery(Conn, Query) of
        {error, {error, error, <<"42P01">>, undefined_table, _, _}} ->
            ok = init(Conn),
            upgrade(Conn, 0, Dir);
        {ok, _, [{Version}]} ->
            Reason = io_lib:format("[Migrate] ~p~n", Version),
            logger:error(Reason),
            upgrade(Conn, binary_to_integer(Version), Dir);
        Otherwise ->
            Reason = io_lib:format("[Migrate] Unmapped Case: ~p~n", Otherwise),
            logger:error(Reason),
            {rollback, Reason}
    end.
