-module(migraterl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([migrate_test/1, idempotent_test/1, dry_run_test/1]).

all() -> [migrate_test, idempotent_test, dry_run_test].

init_per_testcase(_, Config) ->
    _ = application:ensure_all_started(crypto),
    Conn = migraterl:default_connection(),
    Dir = shared:get_test_directory(migraterl),
    Sources = [
        {once, filename:join([Dir, "main"])},
        {always, filename:join([Dir, "repeatable"])}
    ],
    [{conn, Conn}, {sources, Sources} | Config].

end_per_testcase(_, Config) ->
    Conn = ?config(conn, Config),
    Queries = [
        "DROP SCHEMA IF EXISTS migraterl CASCADE;",
        "DROP SCHEMA IF EXISTS test CASCADE;",
        "DROP SCHEMA IF EXISTS ci CASCADE;"
    ],
    _ = [epgsql:squery(Conn, Q) || Q <- Queries],
    ok.

opts(Config) ->
    #{namespace => <<"ct">>, sources => ?config(sources, Config)}.

%% A fresh database applies the two `once' scripts and the one `always' script.
migrate_test(Config) ->
    {ok, Summary} = migraterl:migrate(?config(conn, Config), opts(Config)),
    #{applied := Applied, warnings := Warnings} = Summary,
    ?assertEqual([], Warnings),
    ?assertEqual(3, length(Applied)).

%% Re-running applies no `once' scripts again but re-runs the `always' one.
idempotent_test(Config) ->
    Conn = ?config(conn, Config),
    {ok, _} = migraterl:migrate(Conn, opts(Config)),
    {ok, #{applied := Applied}} = migraterl:migrate(Conn, opts(Config)),
    %% only the repeatable (always) script runs on the second pass
    ?assertEqual(1, length(Applied)),
    {ok, State} = migraterl:status(Conn, <<"ct">>),
    ?assertEqual(2, length(State)).

%% Dry run reports the plan without applying anything.
dry_run_test(Config) ->
    Conn = ?config(conn, Config),
    {ok, #{applied := Applied, planned := Planned}} = migraterl:plan(Conn, opts(Config)),
    ?assertEqual([], Applied),
    %% two `once' scripts plus the one `always' script would run
    ?assertEqual(3, length(Planned)),
    {ok, State} = migraterl:status(Conn, <<"ct">>),
    ?assertEqual(0, length(State)).
