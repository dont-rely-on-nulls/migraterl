-module(migraterl_plan_tests).
-moduledoc "Pure, DB-free unit tests for the diff engine.".

-include_lib("eunit/include/eunit.hrl").
-include("migraterl.hrl").

script(Name, Class, Order, Hash) ->
    #script{
        namespace = <<"t">>,
        name = Name,
        path = Name,
        class = Class,
        order = Order,
        hash = Hash,
        sql = <<"">>
    }.

entry(Name, Class, Hash) ->
    {Name, #entry{namespace = <<"t">>, name = Name, class = Class, hash = Hash, applied_at = <<>>}}.

opts() -> #opts{namespace = <<"t">>}.

reasons(#plan{actions = As}) -> [{S#script.name, R} || #action{script = S, reason = R} <- As].

%% Nothing applied yet: every `once' script is new, in order.
all_new_test() ->
    Scripts = [
        script(<<"001">>, once, 0, <<"a">>),
        script(<<"002">>, once, 1, <<"b">>)
    ],
    Plan = migraterl_plan:build(Scripts, #{}, opts()),
    ?assertEqual([{<<"001">>, new}, {<<"002">>, new}], reasons(Plan)),
    ?assertEqual([], Plan#plan.warnings).

%% Applied and unchanged `once' scripts are skipped.
skip_applied_test() ->
    Scripts = [script(<<"001">>, once, 0, <<"a">>)],
    Journal = maps:from_list([entry(<<"001">>, once, <<"a">>)]),
    Plan = migraterl_plan:build(Scripts, Journal, opts()),
    ?assertEqual([], reasons(Plan)),
    ?assertEqual([<<"001">>], Plan#plan.skipped).

%% Editing a `once' script after it was applied is drift, not a re-run.
drift_on_changed_once_test() ->
    Scripts = [script(<<"001">>, once, 0, <<"b">>)],
    Journal = maps:from_list([entry(<<"001">>, once, <<"a">>)]),
    Plan = migraterl_plan:build(Scripts, Journal, opts()),
    ?assertEqual([], reasons(Plan)),
    ?assertEqual([{drift, <<"001">>}], Plan#plan.warnings).

%% on_change re-runs only when the hash differs.
on_change_test() ->
    Scripts = [
        script(<<"v_a">>, on_change, 0, <<"new">>),
        script(<<"v_b">>, on_change, 1, <<"same">>)
    ],
    Journal = maps:from_list([
        entry(<<"v_a">>, on_change, <<"old">>),
        entry(<<"v_b">>, on_change, <<"same">>)
    ]),
    Plan = migraterl_plan:build(Scripts, Journal, opts()),
    ?assertEqual([{<<"v_a">>, changed}], reasons(Plan)),
    ?assertEqual([<<"v_b">>], Plan#plan.skipped).

%% `always' scripts run every time, regardless of the journal.
always_test() ->
    Scripts = [script(<<"grants">>, always, 0, <<"h">>)],
    Journal = maps:from_list([entry(<<"grants">>, on_change, <<"h">>)]),
    Plan = migraterl_plan:build(Scripts, Journal, opts()),
    ?assertEqual([{<<"grants">>, always}], reasons(Plan)).

%% A new `once' script sorting before an applied one is out-of-order.
out_of_order_test() ->
    Scripts = [
        script(<<"001">>, once, 0, <<"a">>),
        script(<<"003">>, once, 1, <<"c">>)
    ],
    Journal = maps:from_list([entry(<<"003">>, once, <<"c">>)]),
    Plan = migraterl_plan:build(Scripts, Journal, opts()),
    ?assertEqual([{<<"001">>, new}], reasons(Plan)),
    ?assertEqual([{out_of_order, <<"001">>}], Plan#plan.warnings).

%% The ignore policy suppresses the out-of-order warning.
out_of_order_ignore_test() ->
    Scripts = [
        script(<<"001">>, once, 0, <<"a">>),
        script(<<"003">>, once, 1, <<"c">>)
    ],
    Journal = maps:from_list([entry(<<"003">>, once, <<"c">>)]),
    Plan = migraterl_plan:build(Scripts, Journal, (opts())#opts{on_out_of_order = ignore}),
    ?assertEqual([], Plan#plan.warnings).
