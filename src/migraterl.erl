%%-------------------------------------------------------------------
% @doc Library module.
% @todo Leverage erlandono to abstract away the DB clients
% @end
%%-------------------------------------------------------------------
-module(migraterl).

-compile({parse_transform, do}).
-export([default_connection/0, migrate/3]).

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
%% Given a directory, applies only the files not already present on
%% the migration. If the migration table does not yet exist, make sure
%% to create it beforehand.
%% @end
-spec migrate(Conn, Dir, Options) -> Result when
    Conn :: epgsql:connection(),
    Dir :: directory(),
    Options :: options(),
    Reason :: string(),
    Error :: {error, Reason},
    Result :: ok | Error.
migrate(Conn, Dir, Options) ->
    Query = "SELECT COALESCE(MAX(version),0) as last_version FROM migraterl.history",
    do([
        error_m
     || QueryResult = epgsql:squery(Conn, Query),
        {Version, State} <- set_state(QueryResult),
        Migrations <- select_files({Version, State}, Dir, Options),
        Result = epgsql:with_transaction(Conn, fun(_) -> run(Conn, Migrations, []) end),
        case Result of
            {ok, R} -> return(R);
            {rollback, E} -> fail(E);
            _ -> fail("Unknown error")
        end
    ]).

%% @doc
%% Checks wether we already created Migraterl tables, otherwise set
%% the state to be initialized.
%% @end
set_state(QueryResult) ->
    case QueryResult of
        {error, {error, error, <<"42P01">>, undefined_table, _, _}} -> {ok, {0, init}};
        {ok, _, [{V}]} -> {ok, {binary_to_integer(V), created}}
    end.

%% @doc
%% Applies the missing migrations files to the Database.
%% @end
-spec select_files({Version, State}, Dir, Options) -> Result when
    Version :: version(),
    State :: state(),
    Dir :: directory(),
    Options :: options(),
    Reason :: string(),
    Ok :: {ok, [{mode(), filename()}]},
    Error :: {error, Reason},
    Result :: Ok | Error.
select_files({Version, init}, Dir, Options) ->
    do([
        error_m
     || % Pick the files required to bootstrap our tables
        InitFiles <- file_utils:read_system_migrations(),
        InitSeq = init_seq(InitFiles),
        % The migrations yet to be applied
        Files <- file_utils:read_directory(Dir),
        MigrationSeq <- migration_seq(Files, Version, Options),
        Migrations = lists:append(InitSeq, MigrationSeq),
        return(Migrations)
    ]);
select_files({Version, created}, Dir, Options) ->
    do([
        error_m
     || Files <- file_utils:read_directory(Dir),
        Migrations <- migration_seq(Files, Version, Options),
        return(Migrations)
    ]).

init_seq(InitFiles) ->
    InitModes = [setup || _ <- InitFiles],
    lists:zip(InitModes, InitFiles).

migration_seq(Files, Version, #{repeatable := false}) ->
    Subset = lists:nthtail(Version, Files),
    SubsetModes = [apply_once || _ <- Subset],
    List = lists:zip(SubsetModes, Subset),
    {ok, List};
migration_seq(Files, _, #{repeatable := true}) ->
    FileModes = [repeat || _ <- Files],
    List = lists:zip(FileModes, Files),
    {ok, List};
migration_seq(_Files, _Version, _) ->
    {error, "Invalid Option"}.

%% @doc
%% Parses [{mode, filepath}] recursively, if an error happens this
%% will immediatly stop and trigger a rollback.
%% @end
-spec run(Conn, Entry, Acc) -> Result when
    Conn :: epgsql:connection(),
    Entry :: [{mode(), filename()}],
    Acc :: [string()],
    Reason :: string(),
    Ok :: {ok, list()},
    Error :: {error, Reason},
    Result :: Ok | Error.
run(_, [], Acc) ->
    {ok, Acc};
run(Conn, [{Mode, Path} | T], Acc) ->
    Filename = filename:basename(Path),
    {ok, SQL} = aggregate(Path, Filename, Mode),
    QueryResult = epgsql:squery(Conn, SQL),
    case QueryResult of
        {ok, _} ->
            run(Conn, T, lists:append(Acc, [Filename]));
        {ok, _, _} ->
            run(Conn, T, lists:append(Acc, [Filename]));
        L when is_list(L) ->
            case lists:all(fun(X) -> ok =:= element(1, X) end, L) of
                true ->
                    run(Conn, T, lists:append(Acc, [Filename]));
                false ->
                    {rollback, "Failure while processing list"}
            end;
        {error, {_, _, _, ErrorType, ErrorMessage, _}} ->
            Reason = io_lib:format("ERROR at ~s: ~p~n~p~n", [Filename, ErrorType, ErrorMessage]),
            logger:error(Reason),
            {rollback, Reason}
    end.

update_history(File) ->
    Query = "INSERT INTO migraterl.history (filename) VALUES ('~s');",
    SQL = io_lib:format(Query, [File]),
    list_to_binary(SQL).

aggregate(Path, Filename, apply_once) ->
    do([
        error_m
     || Bin <- file:read_file(Path),
        Update = update_history(Filename),
        SQL = <<Bin/binary, Update/binary>>,
        return(SQL)
    ]);
aggregate(Path, _, _) ->
    do([
        error_m
     || Bin <- file:read_file(Path),
        return(Bin)
    ]).
