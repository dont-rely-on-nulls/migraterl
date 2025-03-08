-module(file_utils_test).

-include_lib("eunit/include/eunit.hrl").

format_bin_content_test() ->
    Data = <<"1\n2\n3\n">>,
    Expected = "123",
    {ok, Output} = file_utils:format_bin_content(Data),
    ?assertEqual(Output, Expected).
