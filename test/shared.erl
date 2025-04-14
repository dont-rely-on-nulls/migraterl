-module(shared).

-export([get_test_directory/1]).

get_test_directory(Module) ->
    Dir = code:lib_dir(Module),
    L = filename:split(Dir),
    % small sanity check, if someone changes the main module
    % name, this will break as well.
    ["lib", "migraterl"] = lists:nthtail(length(L) - 2, L),
    PathSuffix = ["test", "migrations"],
    Path = filename:join([Dir | PathSuffix]),
    Path.
