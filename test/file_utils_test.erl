-module(file_utils_test).

%-include("internal_types.hrl").
-include_lib("eunit/include/eunit.hrl").

read_system_migrations_test() ->
    0.

format_bin_content_test() ->
    Bin = <<"1\n2\n3\n">>,
    Expected = "123",
    {ok, Output} = file_utils:format_bin_content(Bin),
    ?assertEqual(Output, Expected).
