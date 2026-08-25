-module(migraterl_config_tests).
-moduledoc "Unit tests for source and lifecycle option normalization.".

-include_lib("eunit/include/eunit.hrl").
-include("migraterl.hrl").

legacy_sources_test() ->
    {ok, Opts} = migraterl_config:normalize(#{
        namespace => test,
        sources => [{once, "one"}, {always, "two"}]
    }),
    ?assertEqual(<<"test">>, Opts#opts.namespace),
    ?assertEqual(
        [
            #source{id = <<"legacy_0">>, stage = legacy, path = "one", class = once},
            #source{id = <<"legacy_1">>, stage = legacy, path = "two", class = always}
        ],
        Opts#opts.sources
    ),
    ?assertEqual(
        lists:sort([filename:absname("one"), filename:absname("two")]),
        Opts#opts.watch_dirs
    ).

grate_profile_test() ->
    Root = "priv/migrations",
    {ok, Opts} = migraterl_config:normalize(#{layout => #{profile => grate, root => Root}}),
    Sources = Opts#opts.sources,
    ?assertEqual(13, length(Sources)),
    ?assertMatch(
        #source{
            id = before_migration,
            stage = before_migration,
            path = "priv/migrations/beforeMigration",
            class = always,
            required = false
        },
        hd(Sources)
    ),
    ?assertMatch(
        #source{
            id = after_migration,
            stage = after_migration,
            path = "priv/migrations/afterMigration",
            class = always,
            required = false
        },
        lists:last(Sources)
    ),
    ?assertEqual([filename:absname(Root)], migraterl_config:watch_dirs(Opts)).

custom_stages_preserve_order_test() ->
    Layout = #{
        root => "db",
        stages => [
            #{id => prepare_source, stage => prepare, path => "prepare", class => always},
            #{
                id => <<"linear_source">>,
                stage => linear,
                path => "up",
                class => once,
                required => false
            },
            #{id => views_source, stage => replaceable, path => "views", class => on_change}
        ]
    },
    {ok, Opts} = migraterl_config:normalize(#{layout => Layout}),
    ?assertEqual(
        [
            #source{
                id = prepare_source,
                stage = prepare,
                path = "db/prepare",
                class = always,
                required = true
            },
            #source{
                id = <<"linear_source">>,
                stage = linear,
                path = "db/up",
                class = once,
                required = false
            },
            #source{
                id = views_source,
                stage = replaceable,
                path = "db/views",
                class = on_change,
                required = true
            }
        ],
        Opts#opts.sources
    ).

conflicting_sources_and_layout_test() ->
    ?assertEqual(
        {error, {invalid_options, conflicting_sources_and_layout}},
        migraterl_config:normalize(#{sources => [], layout => #{profile => grate, root => "db"}})
    ).

conflicting_profile_and_stages_test() ->
    ?assertEqual(
        {error, {invalid_options, {invalid_layout, conflicting_profile_and_stages}}},
        migraterl_config:normalize(#{
            layout => #{profile => grate, root => "db", stages => []}
        })
    ).

duplicate_stage_id_test() ->
    ?assertEqual(
        {error, {invalid_options, {invalid_layout, {duplicate_stage_id, same}}}},
        migraterl_config:normalize(#{
            layout => #{
                root => "db",
                stages => [
                    #{id => same, stage => first, path => "one", class => once},
                    #{id => same, stage => second, path => "two", class => on_change}
                ]
            }
        })
    ).

invalid_custom_stage_test() ->
    Invalid = [
        #{path => "up", class => once},
        #{id => no_stage, path => "up", class => once},
        #{id => bad_class, stage => bad_class, path => "up", class => repeat},
        #{id => absolute, stage => absolute, path => "/tmp/up", class => once},
        #{id => parent, stage => parent, path => "../up", class => once},
        #{id => <<>>, stage => empty_id, path => "up", class => once},
        #{id => '', stage => empty_id_atom, path => "up", class => once},
        #{id => empty_stage, stage => <<>>, path => "up", class => once},
        #{id => empty_stage_atom, stage => '', path => "up", class => once},
        #{id => extra, stage => extra, path => "up", class => once, typo => true}
    ],
    [
        ?assertMatch(
            {error, {invalid_options, {invalid_layout, {invalid_stage, 0, _}}}},
            migraterl_config:normalize(#{layout => #{root => "db", stages => [Stage]}})
        )
     || Stage <- Invalid
    ].

public_api_rejects_layout_before_using_connection_test() ->
    ?assertMatch(
        {error, {invalid_options, _}},
        migraterl:plan(not_a_connection, #{layout => #{profile => unknown, root => "db"}})
    ).

defaults_and_accepted_scalar_values_test() ->
    {ok, Defaults} = migraterl_config:normalize(#{}),
    ?assertEqual(<<"default">>, Defaults#opts.namespace),
    ?assertEqual(per_script, Defaults#opts.txn),
    ?assertEqual(warn, Defaults#opts.on_out_of_order),
    ?assertEqual(#{}, Defaults#opts.variables),
    ?assertEqual(false, Defaults#opts.dry_run),
    ?assertEqual(true, Defaults#opts.notify),
    [
        begin
            {ok, Opts} = migraterl_config:normalize(#{namespace => Input}),
            ?assertEqual(Expected, Opts#opts.namespace)
        end
     || {Input, Expected} <- [
            {<<"binary">>, <<"binary">>},
            {atom_namespace, <<"atom_namespace">>},
            {"unicode_λ", <<"unicode_λ"/utf8>>}
        ]
    ],
    [
        begin
            {ok, Opts} = migraterl_config:normalize(#{txn => Value}),
            ?assertEqual(Value, Opts#opts.txn)
        end
     || Value <- [per_script, single, none]
    ],
    [
        begin
            {ok, Opts} = migraterl_config:normalize(#{on_out_of_order => Value}),
            ?assertEqual(Value, Opts#opts.on_out_of_order)
        end
     || Value <- [warn, error, ignore]
    ],
    {ok, ScalarOpts} = migraterl_config:normalize(#{
        variables => #{<<"key">> => <<"value">>}, dry_run => true, notify => false
    }),
    ?assertEqual(#{<<"key">> => <<"value">>}, ScalarOpts#opts.variables),
    ?assert(ScalarOpts#opts.dry_run),
    ?assertNot(ScalarOpts#opts.notify).

unknown_top_level_keys_are_sorted_test() ->
    ?assertEqual(
        {error, {invalid_options, {unknown_keys, [aaa, zzz]}}},
        migraterl_config:normalize(#{zzz => 1, aaa => 2})
    ).

invalid_scalar_values_test() ->
    Invalid = [
        {#{namespace => 42}, {invalid_namespace, 42}},
        {#{namespace => []}, {invalid_namespace, []}},
        {#{namespace => <<255>>}, {invalid_namespace, <<255>>}},
        {#{namespace => lists:duplicate(256, $a)}, {invalid_namespace, lists:duplicate(256, $a)}},
        {#{namespace => [97 | invalid]}, {invalid_namespace, [97 | invalid]}},
        {#{txn => transaction}, {invalid_txn, transaction}},
        {#{on_out_of_order => stop}, {invalid_on_out_of_order, stop}},
        {#{variables => []}, {invalid_variables, []}},
        {
            #{variables => #{atom_key => <<"value">>}},
            {invalid_variables, #{atom_key => <<"value">>}}
        },
        {
            #{variables => #{<<"key">> => atom_value}},
            {invalid_variables, #{<<"key">> => atom_value}}
        },
        {#{dry_run => yes}, {invalid_dry_run, yes}},
        {#{notify => no}, {invalid_notify, no}}
    ],
    [
        ?assertEqual(
            {error, {invalid_options, Reason}},
            migraterl_config:normalize(Options)
        )
     || {Options, Reason} <- Invalid
    ].

invalid_legacy_sources_test() ->
    Invalid = [
        repeat,
        {once, <<>>},
        {once, <<"bad", 0, "path">>},
        {once, ["nested"]},
        {once, [97 | invalid]},
        not_a_tuple
    ],
    [
        ?assertMatch(
            {error, {invalid_options, {invalid_source, 0, _}}},
            migraterl_config:normalize(#{sources => [Source]})
        )
     || Source <- Invalid
    ],
    ?assertMatch(
        {error, {invalid_options, {invalid_source, 1, _}}},
        migraterl_config:normalize(#{sources => [{once, "ok"} | improper]})
    ),
    ?assertEqual(
        {error, {invalid_options, {invalid_sources, not_a_list}}},
        migraterl_config:normalize(#{sources => not_a_list})
    ).

custom_stage_ids_are_unique_but_stages_may_repeat_test() ->
    Layout = #{
        root => "db",
        stages => [
            #{id => first, stage => shared, path => "one", class => once},
            #{id => second, stage => shared, path => "two", class => on_change}
        ]
    },
    {ok, Opts} = migraterl_config:normalize(#{layout => Layout}),
    ?assertEqual([shared, shared], [Source#source.stage || Source <- Opts#opts.sources]).

layout_validation_envelope_test() ->
    InvalidLayouts = [
        not_a_map,
        #{profile => grate},
        #{root => <<>>, profile => grate},
        #{root => [97 | invalid], profile => grate},
        #{root => "db"},
        #{root => "db", profile => repeat},
        #{root => "db", stages => not_a_list},
        #{root => "db", stages => [#{id => source, stage => stage, path => <<0>>, class => once}]},
        #{root => "db", profile => grate, typo => true}
    ],
    [
        ?assertMatch(
            {error, {invalid_options, {invalid_layout, _}}},
            migraterl_config:normalize(#{layout => Layout})
        )
     || Layout <- InvalidLayouts
    ].

all_invalid_options_precede_connection_use_test() ->
    Invalid = [
        not_a_map,
        #{unknown => value},
        #{namespace => 1},
        #{txn => bad},
        #{on_out_of_order => bad},
        #{variables => bad},
        #{dry_run => bad},
        #{notify => bad},
        #{sources => [{repeat, "db"}]},
        #{layout => #{profile => unknown, root => "db"}}
    ],
    [
        ?assertMatch(
            {error, {invalid_options, _}},
            migraterl:migrate(not_a_connection, Options)
        )
     || Options <- Invalid
    ].
