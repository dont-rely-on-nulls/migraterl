-module(migraterl_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([init_test/1, migrate_test/1]).

all() -> [init_test, migrate_test].

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
    SqlQuery = "DROP SCHEMA IF EXISTS migraterl CASCADE;",
    epgsql:squery(Conn, SqlQuery),
    ets:delete(?config(conn_data, Config)).

% init test cases, make sure to have postgresql running,
% "devenv up" will take care of that in your nix shell.
init_test(Config) ->
    TabId = ?config(conn_data, Config),
    [{connection, Conn}] = ets:lookup(TabId, connection),
    ok = migraterl:init(Conn).

% migrate test cases, uses the example SQL migrations
% from the test/migrations directory.
migrate_test(Config) ->
    TabId = ?config(conn_data, Config),
    [{connection, Conn}] = ets:lookup(TabId, connection),
    [{migration_dir, Dir}] = ets:lookup(TabId, migration_dir),
    MainPath = filename:join([Dir, "main"]),
    ok = migraterl:migrate(Conn, MainPath).
