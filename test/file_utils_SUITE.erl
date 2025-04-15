-module(file_utils_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([format_bin_content_test/1, read_system_migrations_test/1, read_directory_test/1]).

all() -> [read_directory_test, format_bin_content_test, read_system_migrations_test].

% Testing infrastructure
init_per_testcase(format_bin_content_test, Config) ->
    TabName = fbc_data,
    TabId = ets:new(TabName, [ordered_set, public]),
    ets:insert(TabId, {valid_data, <<"SELECT 1;\nSELECT 2;\n">>}),
    ets:insert(TabId, {invalid_data, <<"">>}),
    [{TabName, TabId} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(format_bin_content_test, Config) ->
    ets:delete(?config(fbc_data, Config));
end_per_testcase(_, _Config) ->
    true.

% format_bin_content test cases
format_bin_content_test(Config) ->
    TabId = ?config(fbc_data, Config),
    % Happy path
    [{valid_data, ValidData}] = ets:lookup(TabId, valid_data),
    {ok, "SELECT 1; SELECT 2;"} = file_utils:format_bin_content(ValidData),
    % Error path
    [{invalid_data, InvalidData}] = ets:lookup(TabId, invalid_data),
    {error, _} = file_utils:format_bin_content(InvalidData).

% read_system_migrations test cases
read_system_migrations_test(_Config) ->
    % This should always work, unless you break some API
    {ok, Path} = file_utils:read_system_migrations(),
    L = filename:split(Path),
    ["migraterl", "priv", "system"] = lists:nthtail(length(L) - 3, L).

% read_directory test cases
read_directory_test(_Config) ->
    % Error path
    {error, _} = file_utils:read_directory("./wrong"),
    % Happy path
    Path = shared:get_test_directory(migraterl),
    {ok, _} = file_utils:read_directory(Path).
