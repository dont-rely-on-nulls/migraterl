-module(migraterl_scanner_tests).
-moduledoc "Filesystem scanner tests for normalized lifecycle sources.".

-include_lib("eunit/include/eunit.hrl").
-include("migraterl.hrl").

source_order_test() ->
    with_tmp(fun(Root) ->
        First = make_dir(Root, "first"),
        Second = make_dir(Root, "second"),
        write_sql(First, "002_b.sql"),
        write_sql(First, "001_a.sql"),
        write_sql(Second, "003_c.sql"),
        Sources = [
            #source{id = first, stage = linear, path = First, class = once},
            #source{id = second, stage = replaceable, path = Second, class = on_change}
        ],
        {ok, Scripts} = migraterl_scanner:scan(<<"test">>, Sources),
        ?assertEqual(
            [
                {<<"001_a.sql">>, first, linear, 0},
                {<<"002_b.sql">>, first, linear, 1},
                {<<"003_c.sql">>, second, replaceable, 0}
            ],
            [{S#script.name, S#script.source_id, S#script.stage, S#script.order} || S <- Scripts]
        )
    end).

optional_missing_directory_test() ->
    with_tmp(fun(Root) ->
        Missing = filename:join(Root, "missing"),
        Source = #source{
            id = optional, stage = optional_stage, path = Missing, class = once, required = false
        },
        ?assertEqual({ok, []}, migraterl_scanner:scan(<<"test">>, [Source]))
    end).

required_missing_directory_test() ->
    with_tmp(fun(Root) ->
        Missing = filename:join(Root, "missing"),
        Source = #source{id = required, stage = required_stage, path = Missing, class = once},
        ?assertEqual(
            {error, {list_dir, Missing, enoent}},
            migraterl_scanner:scan(<<"test">>, [Source])
        )
    end).

duplicate_basename_test() ->
    with_tmp(fun(Root) ->
        First = make_dir(Root, "first"),
        Second = make_dir(Root, "second"),
        FirstPath = write_sql(First, "same.sql"),
        SecondPath = write_sql(Second, "same.sql"),
        Sources = [
            #source{id = first, stage = before, path = First, class = always},
            #source{id = second, stage = linear, path = Second, class = once}
        ],
        ?assertEqual(
            {error,
                {
                    duplicate_script_name,
                    <<"same.sql">>,
                    [filename:absname(FirstPath), filename:absname(SecondPath)]
                }},
            migraterl_scanner:scan(<<"test">>, Sources)
        )
    end).

binary_source_path_test() ->
    with_tmp(fun(Root) ->
        Dir = make_dir(Root, "binary"),
        _ = write_sql(Dir, "001.sql"),
        Source = #source{
            id = binary_source,
            stage = linear,
            path = list_to_binary(Dir),
            class = once
        },
        {ok, [Script]} = migraterl_scanner:scan(<<"test">>, [Source]),
        ?assertEqual(<<"001.sql">>, Script#script.name),
        ?assertEqual(binary_source, Script#script.source_id),
        ?assertEqual(linear, Script#script.stage)
    end).

read_failure_test() ->
    with_tmp(fun(Root) ->
        Dir = make_dir(Root, "source"),
        SqlDirectory = make_dir(Dir, "unreadable.sql"),
        Source = #source{id = source, stage = linear, path = Dir, class = once},
        ?assertEqual(
            {error, {read_error, filename:absname(SqlDirectory), eisdir}},
            migraterl_scanner:scan(<<"test">>, [Source])
        )
    end).

with_tmp(Test) ->
    Root = filename:join(
        "/tmp", "migraterl_scanner_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Root),
    try
        Test(Root)
    after
        ok = file:del_dir_r(Root)
    end.

make_dir(Root, Name) ->
    Dir = filename:join(Root, Name),
    ok = file:make_dir(Dir),
    Dir.

write_sql(Dir, Name) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, <<"SELECT 1;">>),
    Path.
