-module(migraterl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    legacy_contract_test/1,
    source_order_and_duplicate_preflight_test/1,
    once_drift_and_out_of_order_test/1,
    variables_and_redacted_failure_log_test/1,
    dry_run_equivalence_test/1,
    transaction_modes_test/1,
    empty_and_full_profile_test/1,
    custom_layout_equivalence_test/1,
    stage_failure_suppresses_later_stages_test/1,
    structured_scan_failure_log_test/1
]).

all() ->
    [
        legacy_contract_test,
        source_order_and_duplicate_preflight_test,
        once_drift_and_out_of_order_test,
        variables_and_redacted_failure_log_test,
        dry_run_equivalence_test,
        transaction_modes_test,
        empty_and_full_profile_test,
        custom_layout_equivalence_test,
        stage_failure_suppresses_later_stages_test,
        structured_scan_failure_log_test
    ].

init_per_testcase(TestCase, Config) ->
    _ = application:ensure_all_started(crypto),
    Conn =
        case migraterl:default_connection() of
            Pid when is_pid(Pid) -> Pid;
            Error -> ct:fail({connection_failed, Error})
        end,
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    Root = filename:join("/tmp", "migraterl_ct_" ++ atom_to_list(TestCase) ++ "_" ++ Suffix),
    ok = file:make_dir(Root),
    Namespace = iolist_to_binary([atom_to_list(TestCase), "_", Suffix]),
    [{conn, Conn}, {root, Root}, {namespace, Namespace} | Config].

end_per_testcase(_, Config) ->
    _ = logger:remove_handler(migraterl_ct_capture),
    Conn = ?config(conn, Config),
    Schemas = [
        "migraterl",
        "fixture",
        "duplicate_marker",
        "drift_marker",
        "ooo_warn",
        "ooo_error",
        "ooo_ignore",
        "variable_fixture",
        "dry_a",
        "dry_b",
        "dry_c",
        "tx_per_script",
        "tx_single",
        "tx_none",
        "stage_before",
        "stage_after"
    ],
    _ = [
        epgsql:squery(Conn, "DROP SCHEMA IF EXISTS " ++ Schema ++ " CASCADE")
     || Schema <- Schemas
    ],
    _ = epgsql:close(Conn),
    ok = file:del_dir_r(?config(root, Config)),
    ok.

%% Freeze the tagged v0.5 contract: all three classes, status shape, temporal
%% on_change history, and the fact that always is never journaled.
legacy_contract_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    OnceSql = <<"CREATE SCHEMA fixture; CREATE TABLE fixture.once_marker(id int);">>,
    ChangeSql =
        <<"CREATE OR REPLACE FUNCTION fixture.answer() RETURNS int LANGUAGE sql AS 'SELECT 42';">>,
    AlwaysSql = <<
        "CREATE TABLE IF NOT EXISTS fixture.always_runs(id int); "
        "INSERT INTO fixture.always_runs VALUES (1);"
    >>,
    Once = source_dir(Root, "once", [{"001_once.sql", OnceSql}]),
    Change = source_dir(Root, "change", [{"010_change.sql", ChangeSql}]),
    Always = source_dir(Root, "always", [{"020_always.sql", AlwaysSql}]),
    Opts = #{namespace => Ns, sources => [{once, Once}, {on_change, Change}, {always, Always}]},
    Expected = [<<"001_once.sql">>, <<"010_change.sql">>, <<"020_always.sql">>],
    {ok, #{planned := Expected, applied := Expected, warnings := []}} = migraterl:migrate(
        Conn, Opts
    ),
    {ok, State} = migraterl:status(Conn, Ns),
    ?assertEqual(2, length(State)),
    ?assertEqual([applied_at, class, hash, name], lists:sort(maps:keys(hd(State)))),
    ?assertEqual([on_change, once], lists:sort([maps:get(class, Row) || Row <- State])),
    ?assertEqual(
        [
            {<<"001_once.sql">>, hash(OnceSql), <<"once">>, <<"sha256">>},
            {<<"010_change.sql">>, hash(ChangeSql), <<"on_change">>, <<"sha256">>}
        ],
        journal_current(Conn, Ns)
    ),
    ChangeSql2 = <<ChangeSql/binary, "\n-- changed">>,
    ok = file:write_file(filename:join(Change, "010_change.sql"), ChangeSql2),
    {ok, #{applied := [<<"010_change.sql">>, <<"020_always.sql">>]}} =
        migraterl:migrate(Conn, Opts),
    ?assertEqual(2, scalar_int(Conn, "SELECT count(*) FROM fixture.always_runs")),
    ?assertEqual(
        [
            {<<"once">>, <<"sha256">>},
            {<<"on_change">>, <<"sha256">>},
            {<<"on_change">>, <<"sha256">>}
        ],
        query_pairs(
            Conn,
            "SELECT script_class, checksum_algo FROM migraterl.schema_journal ORDER BY script_name, lower(valid_period)"
        )
    ).

source_order_and_duplicate_preflight_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    _ = epgsql:squery(Conn, "CREATE SCHEMA fixture; CREATE TABLE fixture.order_log(position int)"),
    First = source_dir(Root, "first", [
        {"002_second.sql", <<"INSERT INTO fixture.order_log VALUES (2);">>},
        {"001_first.sql", <<"INSERT INTO fixture.order_log VALUES (1);">>}
    ]),
    Second = source_dir(Root, "second", [
        {"000_third.sql", <<"INSERT INTO fixture.order_log VALUES (3);">>}
    ]),
    Ordered = #{namespace => Ns, sources => [{once, First}, {once, Second}]},
    Expected = [<<"001_first.sql">>, <<"002_second.sql">>, <<"000_third.sql">>],
    {ok, #{planned := Expected, applied := Expected}} = migraterl:migrate(Conn, Ordered),
    ?assertEqual(
        [1, 2, 3], query_ints(Conn, "SELECT position FROM fixture.order_log ORDER BY ctid")
    ),

    DupA = source_dir(Root, "dup_a", [{"same.sql", <<"CREATE SCHEMA duplicate_marker;">>}]),
    DupB = source_dir(Root, "dup_b", [{"same.sql", <<"CREATE SCHEMA duplicate_marker;">>}]),
    ?assertMatch(
        {error, {duplicate_script_name, <<"same.sql">>, [_, _]}},
        migraterl:migrate(Conn, #{
            namespace => <<Ns/binary, "_dup">>,
            sources => [
                {always, DupA}, {once, DupB}
            ]
        })
    ),
    ?assertEqual(<<"t">>, scalar(Conn, "SELECT to_regnamespace('duplicate_marker') IS NULL")).

once_drift_and_out_of_order_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    Drift = source_dir(Root, "drift", [{"001.sql", <<"CREATE SCHEMA drift_marker;">>}]),
    DriftOpts = #{namespace => <<Ns/binary, "_drift">>, sources => [{once, Drift}]},
    {ok, _} = migraterl:migrate(Conn, DriftOpts),
    ok = file:write_file(
        filename:join(Drift, "001.sql"), <<"CREATE SCHEMA drift_marker; -- edited">>
    ),
    {ok, #{applied := [], warnings := [{drift, <<"001.sql">>}]}} =
        migraterl:migrate(Conn, DriftOpts),

    test_out_of_order(Conn, Root, <<Ns/binary, "_warn">>, warn, "ooo_warn"),
    test_out_of_order(Conn, Root, <<Ns/binary, "_error">>, error, "ooo_error"),
    test_out_of_order(Conn, Root, <<Ns/binary, "_ignore">>, ignore, "ooo_ignore").

variables_and_redacted_failure_log_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    Secret = <<"do-not-log-this-value">>,
    Success = source_dir(Root, "variables", [
        {"001.sql", <<
            "CREATE SCHEMA variable_fixture; "
            "CREATE TABLE variable_fixture.values(value text); "
            "INSERT INTO variable_fixture.values VALUES ('$secret$');"
        >>}
    ]),
    {ok, _} = migraterl:migrate(Conn, #{
        namespace => <<Ns/binary, "_success">>,
        sources => [{once, Success}],
        variables => #{<<"secret">> => Secret}
    }),
    ?assertEqual(Secret, scalar(Conn, "SELECT value FROM variable_fixture.values")),

    ok = add_log_capture(),
    Failure = source_dir(Root, "failure", [{"002_fail.sql", <<"SELECT 1 / 0; -- $secret$">>}]),
    Layout = #{
        root => Root,
        stages => [
            #{
                id => failure_source,
                stage => linear,
                path => filename:basename(Failure),
                class => once
            }
        ]
    },
    FailureNs = <<Ns/binary, "_failure">>,
    Result = migraterl:migrate(Conn, #{
        namespace => FailureNs,
        layout => Layout,
        variables => #{<<"secret">> => Secret}
    }),
    ?assertMatch({error, {apply_failed, <<"002_fail.sql">>, _}}, Result),
    Event = receive_log(),
    Meta = maps:get(meta, Event),
    ?assertEqual(
        #{
            namespace => FailureNs,
            script => <<"002_fail.sql">>,
            path => filename:absname(filename:join(Failure, "002_fail.sql")),
            stage => linear,
            source_id => failure_source,
            failure => apply_failed
        },
        maps:with([namespace, script, path, stage, source_id, failure], Meta)
    ),
    ?assertEqual(nomatch, binary:match(render(Result), Secret)),
    ?assertEqual(nomatch, binary:match(render(Event), Secret)).

dry_run_equivalence_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    First = source_dir(Root, "dry_first", [
        {"002.sql", <<"CREATE SCHEMA dry_b;">>},
        {"001.sql", <<"CREATE SCHEMA dry_a;">>}
    ]),
    Second = source_dir(Root, "dry_second", [{"000.sql", <<"CREATE SCHEMA dry_c;">>}]),
    Opts = #{namespace => Ns, sources => [{once, First}, {always, Second}]},
    Expected = [<<"001.sql">>, <<"002.sql">>, <<"000.sql">>],
    {ok, #{planned := Expected, applied := []}} = migraterl:plan(Conn, Opts),
    [
        ?assertEqual(<<"t">>, scalar(Conn, "SELECT to_regnamespace('" ++ S ++ "') IS NULL"))
     || S <- ["dry_a", "dry_b", "dry_c"]
    ],
    ?assertEqual(0, journal_count(Conn, Ns)),
    {ok, #{planned := Expected, applied := Expected}} = migraterl:migrate(Conn, Opts).

transaction_modes_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    test_transaction_mode(Conn, Root, <<Ns/binary, "_per">>, per_script, "tx_per_script"),
    test_transaction_mode(Conn, Root, <<Ns/binary, "_single">>, single, "tx_single"),
    test_transaction_mode(Conn, Root, <<Ns/binary, "_none">>, none, "tx_none").

empty_and_full_profile_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    Empty = make_dir(Root, "empty_profile"),
    {ok, #{planned := [], applied := [], skipped := [], warnings := []}} =
        migraterl:migrate(Conn, #{
            namespace => <<Ns/binary, "_empty">>,
            layout => #{
                profile => grate, root => Empty
            }
        }),

    Full = make_dir(Root, "full_profile"),
    Profile = [
        {"beforeMigration", "130_before.sql"},
        {"alterDatabase", "120_alter.sql"},
        {"runBeforeUp", "110_before_up.sql"},
        {"up", "100_up.sql"},
        {"runFirstAfterUp", "090_first_after.sql"},
        {"functions", "080_functions.sql"},
        {"views", "070_views.sql"},
        {"sprocs", "060_sprocs.sql"},
        {"triggers", "050_triggers.sql"},
        {"indexes", "040_indexes.sql"},
        {"runAfterOtherAnyTimeScripts", "030_after_change.sql"},
        {"permissions", "020_permissions.sql"},
        {"afterMigration", "010_after.sql"}
    ],
    _ = [source_dir(Full, Dir, [{File, <<"SELECT 1;">>}]) || {Dir, File} <- Profile],
    Expected = [list_to_binary(File) || {_Dir, File} <- Profile],
    FullNs = <<Ns/binary, "_full">>,
    FullOpts = #{namespace => FullNs, layout => #{profile => grate, root => Full}},
    {ok, #{planned := Expected, applied := Expected}} = migraterl:migrate(Conn, FullOpts),
    {ok, #{applied := Reapplied}} = migraterl:migrate(Conn, FullOpts),
    ?assertEqual(
        [<<"130_before.sql">>, <<"020_permissions.sql">>, <<"010_after.sql">>],
        Reapplied
    ),
    ?assertEqual(10, journal_count(Conn, FullNs)).

custom_layout_equivalence_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    LegacyRoot = make_dir(Root, "legacy_equivalent"),
    CustomRoot = make_dir(Root, "custom_equivalent"),
    Classes = [
        {once, "once", "001.sql"},
        {on_change, "change", "002.sql"},
        {always, "always", "003.sql"}
    ],
    LegacyDirs = [
        source_dir(LegacyRoot, Dir, [{File, <<"SELECT 1;">>}])
     || {_Class, Dir, File} <- Classes
    ],
    _ = [
        source_dir(CustomRoot, Dir, [{File, <<"SELECT 1;">>}])
     || {_Class, Dir, File} <- Classes
    ],
    LegacyOpts = #{
        namespace => <<Ns/binary, "_legacy">>,
        sources =>
            lists:zipwith(fun({Class, _, _}, Dir) -> {Class, Dir} end, Classes, LegacyDirs)
    },
    CustomStages = [
        #{id => source_once, stage => lifecycle, path => "once", class => once},
        #{id => source_change, stage => lifecycle, path => "change", class => on_change},
        #{id => source_always, stage => lifecycle, path => "always", class => always}
    ],
    CustomOpts = #{
        namespace => <<Ns/binary, "_custom">>,
        layout => #{
            root => CustomRoot, stages => CustomStages
        }
    },
    {ok, LegacyFirst} = migraterl:migrate(Conn, LegacyOpts),
    {ok, CustomFirst} = migraterl:migrate(Conn, CustomOpts),
    ?assertEqual(LegacyFirst, CustomFirst),
    {ok, LegacySecond} = migraterl:migrate(Conn, LegacyOpts),
    {ok, CustomSecond} = migraterl:migrate(Conn, CustomOpts),
    ?assertEqual(LegacySecond, CustomSecond),
    ?assertEqual(
        journal_classes(Conn, <<Ns/binary, "_legacy">>),
        journal_classes(Conn, <<Ns/binary, "_custom">>)
    ).

stage_failure_suppresses_later_stages_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    _ = source_dir(Root, "before", [{"001.sql", <<"CREATE SCHEMA stage_before;">>}]),
    _ = source_dir(Root, "linear", [{"002.sql", <<"SELECT 1 / 0;">>}]),
    _ = source_dir(Root, "after", [{"003.sql", <<"CREATE SCHEMA stage_after;">>}]),
    Stages = [
        #{id => before_source, stage => before, path => "before", class => always},
        #{id => linear_source, stage => linear, path => "linear", class => once},
        #{id => after_source, stage => 'after', path => "after", class => always}
    ],
    ?assertMatch(
        {error, {apply_failed, <<"002.sql">>, _}},
        migraterl:migrate(Conn, #{namespace => Ns, layout => #{root => Root, stages => Stages}})
    ),
    ?assertEqual(<<"t">>, scalar(Conn, "SELECT to_regnamespace('stage_before') IS NOT NULL")),
    ?assertEqual(<<"t">>, scalar(Conn, "SELECT to_regnamespace('stage_after') IS NULL")).

structured_scan_failure_log_test(Config) ->
    Conn = ?config(conn, Config),
    Root = ?config(root, Config),
    Ns = ?config(namespace, Config),
    Missing = filename:join(Root, "missing"),
    ok = add_log_capture(),
    ?assertEqual(
        {error, {list_dir, Missing, enoent}},
        migraterl:migrate(Conn, #{namespace => Ns, sources => [{once, Missing}]})
    ),
    Meta = maps:get(meta, receive_log()),
    ?assertEqual(
        #{
            namespace => Ns,
            script => undefined,
            path => Missing,
            stage => legacy,
            source_id => <<"legacy_0">>,
            failure => list_dir
        },
        maps:with([namespace, script, path, stage, source_id, failure], Meta)
    ).

test_out_of_order(Conn, Root, Namespace, Policy, Schema) ->
    Dir = source_dir(Root, Schema, [{"002.sql", <<"SELECT 2;">>}]),
    Opts = #{namespace => Namespace, sources => [{once, Dir}], on_out_of_order => Policy},
    {ok, _} = migraterl:migrate(Conn, Opts),
    Sql = iolist_to_binary(["CREATE SCHEMA ", Schema, ";"]),
    ok = file:write_file(filename:join(Dir, "001.sql"), Sql),
    case Policy of
        warn ->
            {ok, #{applied := [<<"001.sql">>], warnings := [{out_of_order, <<"001.sql">>}]}} =
                migraterl:migrate(Conn, Opts),
            ?assertEqual(
                <<"t">>, scalar(Conn, "SELECT to_regnamespace('" ++ Schema ++ "') IS NOT NULL")
            );
        error ->
            ?assertEqual(
                {error, {out_of_order, [<<"001.sql">>]}},
                migraterl:migrate(Conn, Opts)
            ),
            ?assertEqual(
                <<"t">>, scalar(Conn, "SELECT to_regnamespace('" ++ Schema ++ "') IS NULL")
            );
        ignore ->
            {ok, #{applied := [<<"001.sql">>], warnings := []}} = migraterl:migrate(Conn, Opts),
            ?assertEqual(
                <<"t">>, scalar(Conn, "SELECT to_regnamespace('" ++ Schema ++ "') IS NOT NULL")
            )
    end.

test_transaction_mode(Conn, Root, Namespace, Mode, Schema) ->
    Dir = source_dir(Root, Schema, [
        {"001_ok.sql",
            iolist_to_binary([
                "CREATE SCHEMA ",
                Schema,
                "; CREATE TABLE ",
                Schema,
                ".effects(value int); INSERT INTO ",
                Schema,
                ".effects VALUES (1);"
            ])},
        {"002_fail.sql",
            iolist_to_binary([
                "INSERT INTO ", Schema, ".effects VALUES (2); SELECT 1 / 0;"
            ])},
        {"003_later.sql",
            iolist_to_binary([
                "INSERT INTO ", Schema, ".effects VALUES (3);"
            ])}
    ]),
    Result = migraterl:migrate(Conn, #{
        namespace => Namespace, sources => [{once, Dir}], txn => Mode
    }),
    ?assertMatch({error, {apply_failed, <<"002_fail.sql">>, _}}, Result),
    case Mode of
        single ->
            ?assertEqual(
                <<"t">>, scalar(Conn, "SELECT to_regnamespace('" ++ Schema ++ "') IS NULL")
            ),
            ?assertEqual(0, journal_count(Conn, Namespace));
        _ ->
            ?assertEqual(
                [1],
                query_ints(Conn, "SELECT value FROM " ++ Schema ++ ".effects ORDER BY value")
            ),
            ?assertEqual([{<<"001_ok.sql">>, <<"once">>}], journal_names(Conn, Namespace))
    end.

source_dir(Root, Name, Files) ->
    Dir = make_dir(Root, Name),
    _ = [write_sql(Dir, File, Sql) || {File, Sql} <- Files],
    Dir.

make_dir(Root, Name) ->
    Dir = filename:join(Root, Name),
    ok = file:make_dir(Dir),
    Dir.

write_sql(Dir, Name, Sql) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Sql),
    Path.

scalar(Conn, Query) ->
    {ok, _, [{Value}]} = epgsql:squery(Conn, Query),
    Value.

scalar_int(Conn, Query) ->
    to_integer(scalar(Conn, Query)).

query_ints(Conn, Query) ->
    {ok, _, Rows} = epgsql:squery(Conn, Query),
    [binary_to_integer(Value) || {Value} <- Rows].

query_pairs(Conn, Query) ->
    {ok, _, Rows} = epgsql:squery(Conn, Query),
    Rows.

journal_current(Conn, Namespace) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT script_name, content_hash, script_class, checksum_algo "
        "FROM migraterl.schema_journal "
        "WHERE namespace = $1 AND upper(valid_period) IS NULL ORDER BY script_name",
        [Namespace]
    ),
    Rows.

journal_names(Conn, Namespace) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT script_name, script_class FROM migraterl.schema_journal "
        "WHERE namespace = $1 AND upper(valid_period) IS NULL ORDER BY script_name",
        [Namespace]
    ),
    Rows.

journal_classes(Conn, Namespace) ->
    [Class || {_Name, Class} <- journal_names(Conn, Namespace)].

journal_count(Conn, Namespace) ->
    {ok, _, [{Count}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM migraterl.schema_journal WHERE namespace = $1",
        [Namespace]
    ),
    to_integer(Count).

to_integer(Value) when is_integer(Value) -> Value;
to_integer(Value) when is_binary(Value) -> binary_to_integer(Value).

hash(Bin) ->
    Digest = crypto:hash(sha256, Bin),
    list_to_binary([io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Digest]).

add_log_capture() ->
    _ = logger:remove_handler(migraterl_ct_capture),
    logger:add_handler(migraterl_ct_capture, migraterl_test_log_h, #{
        level => error, config => #{owner => self()}
    }).

receive_log() ->
    receive
        {migraterl_test_log, Event} -> Event
    after 2000 ->
        ct:fail(missing_structured_log)
    end.

render(Term) ->
    iolist_to_binary(io_lib:format("~0p", [Term])).
