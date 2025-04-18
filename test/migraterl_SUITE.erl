-module(migraterl_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([migrate_test/1]).

all() -> [migrate_test].

% Testing infrastructure
init_per_testcase(_, Config) ->
    TabName = conn_data,
    TabId = ets:new(TabName, [ordered_set, public]),
    Conn = migraterl:default_connection(),
    Dir = shared:get_test_directory(migraterl),
    ets:insert(TabId, {connection, Conn}),
    ets:insert(TabId, {migration_dir, Dir}),
    [{TabName, TabId} | Config].

end_per_testcase(_, Config) ->
    TabId = ?config(conn_data, Config),
    [{connection, Conn}] = ets:lookup(TabId, connection),
    Queries = [
        "DROP SCHEMA IF EXISTS migraterl CASCADE;",
        "DROP SCHEMA IF EXISTS test CASCADE;",
        "DROP SCHEMA IF EXISTS ci CASCADE;"
    ],
    _Ignore = [epgsql:squery(Conn, Q) || Q <- Queries],
    ets:delete(?config(conn_data, Config)).

% migrate test cases, uses the example SQL migrations
% from the test/migrations directory.
migrate_test(Config) ->
    TabId = ?config(conn_data, Config),
    [{connection, Conn}] = ets:lookup(TabId, connection),
    [{migration_dir, Dir}] = ets:lookup(TabId, migration_dir),
    MainPath = filename:join([Dir, "main"]),
    RepeatablePath = filename:join([Dir, "repeatable"]),
    % First test the normal migrations
    RepeatableTurnedOff = #{repeatable => false},
    {ok, L} = migraterl:migrate(Conn, MainPath, RepeatableTurnedOff),
    3 = length(L),
    % Now test if the repeatable stuff works as well
    RepeatableTurnedOn = #{repeatable => true},
    {ok, R} = migraterl:migrate(Conn, RepeatablePath, RepeatableTurnedOn),
    1 = length(R).
