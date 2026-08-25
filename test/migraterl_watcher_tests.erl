-module(migraterl_watcher_tests).
-moduledoc "Watcher initialization tests for normalized lifecycle options.".

-include_lib("eunit/include/eunit.hrl").

invalid_layout_stops_before_connection_test() ->
    with_trap_exit(fun() ->
        Spec = #{
            conn => not_used,
            migrate => #{sources => [], layout => #{profile => grate, root => "db"}}
        },
        ?assertEqual(
            {stop, {
                invalid_migrate_options,
                {invalid_options, conflicting_sources_and_layout}
            }},
            migraterl_watcher:init(Spec)
        )
    end).

missing_layout_root_cannot_be_watched_test() ->
    with_trap_exit(fun() ->
        Root = filename:join(
            "/tmp", "missing_migraterl_watch_" ++ integer_to_list(erlang:unique_integer([positive]))
        ),
        Spec = #{
            conn => not_used,
            migrate => #{layout => #{profile => grate, root => Root}}
        },
        ?assertEqual(
            {stop, {watch_failed, iolist_to_binary(filename:absname(Root)), enoent}},
            migraterl_watcher:init(Spec)
        )
    end).

with_trap_exit(Test) ->
    Previous = process_flag(trap_exit, false),
    try
        Test()
    after
        process_flag(trap_exit, Previous)
    end.
