%%-------------------------------------------------------------------
% @doc Internal module, handy functions for file and directory
%      manipulation.
% @end
%%-------------------------------------------------------------------
-module(file_utils).

-export([read_system_migrations/0, format_bin_content/1, read_directory/1]).

-include("file_utils.hrl").
-include("migraterl.hrl").

-define(PRIV_DIR_MODULE, migraterl).

-spec read_directory(Dir :: directory()) -> Result when
    Reason :: string(),
    Error :: {error, read_directory_failure, Reason},
    Ok :: {ok, [filename()]},
    Result :: Ok | Error.
read_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            List = lists:map(fun(File) -> filename:join(Dir, File) end, Files),
            Sorted = lists:sort(List),
            {ok, Sorted};
        Otherwise ->
            Message = io_lib:format("Error while reading directory ~p at ~p~n", Dir, Otherwise),
            {error, read_directory_failure, Message}
    end.

-spec read_system_migrations() -> Result when
    Error :: {error, read_directory_failure, string()},
    Ok :: {ok, directory()},
    Result :: Ok | Error.
read_system_migrations() ->
    case code:priv_dir(?PRIV_DIR_MODULE) of
        {error, Reason} ->
            {error, read_directory_failure, Reason};
        Dir ->
            Path = filename:join(Dir, "system"),
            {ok, Path}
    end.

-spec format_bin_content(Bin) -> Result when
    Bin :: binary(),
    Ok :: {ok, sql()},
    Error :: {error, empty_sql_file, Reason :: string()},
    Result :: Ok | Error.
format_bin_content(Bin) ->
    RemoveLineBreaks = binary:replace(Bin, <<"\n">>, <<>>, [global]),
    L = binary:split(RemoveLineBreaks, <<";">>, [global]),
    SQL = lists:map(fun(X) -> string:concat(unicode:characters_to_list(X), ";") end, L),
    case SQL of
        [";"] ->
            Message = "The provided file is empty",
            {error, empty_sql_file, Message};
        _ ->
            {ok, SQL}
    end.
