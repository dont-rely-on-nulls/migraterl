-module(migraterl_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([init_test/1]).

all() -> [init_test].

% Testing infrastructure
init_per_testcase(_, Config) ->
    TabName = conn_data,
    TabId = ets:new(TabName, [ordered_set, public]),
    Conn = migraterl:default_connection(),
    ets:insert(TabId, {connection, Conn}),
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
