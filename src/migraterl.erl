%%-------------------------------------------------------------------
% @doc Library module.
% @end
%%-------------------------------------------------------------------
-module(migraterl).

-export([default_connection/0, init/2, migrate/3]).

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
    Reason :: string(),
    Error :: {error, Reason},
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
            {error, Message}
    end.

%% @doc
%% Collects all SQL statements from a list for Files
%% @end
aggregate(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, SQL} = file_utils:format_bin_content(Bin),
    SQL.

%% @doc
%% Extra check when epgsql outputs a list
%% @end
proper([]) -> true;
proper([{ok, _} | T]) -> proper(T);
proper([{ok, _, _} | T]) -> proper(T);
proper(_) -> false.

%% @doc
%% @todo Replace this with erlandono
%% @end
collapse(_, [], IfOk, _) -> IfOk;
collapse(Fun, [H | T], IfOk, IfError) ->
    case Fun(H) of
        {ok, _} -> collapse(Fun, T, IfOk, IfError);
        {ok, _, _} -> collapse(Fun, T, IfOk, IfError);
        L when is_list(L) ->
            case proper(L) of
                true -> collapse(Fun, T, IfOk, IfError);
                false -> 
                    Reason = io_lib:format("Bad Migration on ~s", L),
                    logger:error("[COLLAPSE] ERROR: ~s~n", [Reason]),
                    IfError(Reason)
            end;
        _ -> 
            Reason = io_lib:format("Bad Migration on ~s", H),
            logger:error("[COLLAPSE] ERROR: ~s~n", [Reason]),
            IfError(Reason)
    end.

%% @doc
%% Inserts into the migraterl history table
%% @todo rewrite this, it sucks
%% @end
update_history(Migrations) ->
    Names = lists:map(fun(F) -> filename:rootname(filename:basename(F)) end, Migrations),
    Values = lists:map(fun(X) -> io_lib:format("(~s)", [X]) end, Names),
    SQL = io_lib:format("INSERT INTO migraterl.history (filename) VALUES ~s;", Values),
    SQL.

%% @doc
%% Applies a list of SQLStatements, either works or sends a rollback (with a reason) to eqpgsql.
%% @end
-spec run(Conn, Statements) -> Result when
    Conn :: epgsql:connection(),
    Statements :: [string()],
    Reason :: string(),
    Error :: {rollback, Reason},
    Result :: ok | Error.
run(_, []) -> ok;
run(Conn, Statements) ->
    Fun = fun (Q) -> epgsql:squery(Conn, Q) end,
    IfError = fun (Reason) -> {rollback, Reason} end,
    collapse(Fun, Statements, ok, IfError).

-spec create_runner(Conn, Statements, Update, Options) -> Fun when
    Conn :: epgsql:connection(),
    Statements :: [string()],
    Update :: string(),
    Options :: options(),
    Reason :: string(),
    Error :: {rollback, Reason},
    Result :: ok | Error,
    Fun :: fun((any()) -> fun((Conn, Statements) -> Result)).
create_runner(Conn, Statements, _, #{repeatable := true}) ->
    fun(_) -> run(Conn, Statements) end;
create_runner(Conn, Statements, _, #{}) ->
    fun(_) -> run(Conn, Statements) end;
create_runner(Conn, Statements, Update, #{repeatable := false}) ->
    New = lists:append(Statements, Update),
    fun(_) -> run(Conn, New) end.

%% @doc Applies the missing migrations files to the Database.
%% @end
-spec upgrade(Conn, Version, Dir, Options) -> Result when
    Conn :: epgsql:connection(),
    Version :: version(),
    Dir :: directory(),
    Options :: options(),
    Reason :: string(),
    Other :: term(),
    Ok :: {ok, Other},
    Error :: {error, Reason},
    Result :: Ok | Error.
upgrade(Conn, Version, Dir, Options) ->
    {ok, Files} = file_utils:read_directory(Dir),
    Migrations = lists:nthtail(Version, Files),
    UpdateHistory = update_history(Migrations),
    Statements = lists:map(fun(F) -> aggregate(F) end, Migrations),
    Fun = create_runner(Conn, Statements, UpdateHistory, Options),
    case epgsql:with_transaction(Conn, Fun) of
        {rollback, Reason} ->
            logger:error("[UPGRADE] ERROR: ~s~n", [Reason]),
            {error, Reason};
        _Other ->
            ok
    end.

%% @doc Creates the required migraterl tables on the Database.
%% @end
-spec init(Conn, Options) -> Result when
    Conn :: epgsql:connection(),
    Options :: options(),
    Error :: {error, any(), any()},
    Result :: ok | Error.
init(Conn, Options) ->
    {ok, Dir} = file_utils:read_system_migrations(),
    upgrade(Conn, 0, Dir, Options).

%% @doc
%% Given a directory, applies only the files not already present on
%% the migration. If the migration table does not yet exist, make sure
%% to create it beforehand.
%% @end
-spec migrate(Conn, Dir, Options) -> Result when
    Conn :: epgsql:connection(),
    Dir :: directory(),
    Options :: options(),
    Error :: {error, any(), any()},
    Result :: ok | Error.
migrate(Conn, Dir, Options) ->
    Query = "SELECT COALESCE(MAX(version),0) as last_version FROM migraterl.history",
    case epgsql:squery(Conn, Query) of
        {error, {error, error, <<"42P01">>, undefined_table, _, _}} ->
            init(Conn, #{});
        {ok, _, [{Version}]} ->
            upgrade(Conn, binary_to_integer(Version), Dir, Options);
        Otherwise ->
            Reason = io_lib:format("[Migrate] Unmapped Case: ~p~n", Otherwise),
            logger:error(Reason),
            {rollback, Reason}
    end.
