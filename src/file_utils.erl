%%-------------------------------------------------------------------
% @doc Internal module, handy functions for file and directory
%      manipulation.
% @end
%%-------------------------------------------------------------------
-module(file_utils).

-export([read_system_migrations/1, format_bin_content/1, read_directory/1]).

-include("internal_types.hrl").
-include("lib_types.hrl").

-spec profile_path(Dir :: directory()) -> directory().
profile_path(Dir) ->
    filename:dirname(
        filename:dirname(Dir)
    ).

-spec database_path(ModuleName :: atom(), Dir :: directory()) -> directory().
database_path(ModuleName, Dir) ->
    Release = profile_path(Dir),
    Path = ["rel", ModuleName, "database"],
    filename:join([Release | Path]).

-spec read_directory(Dir :: directory()) -> Result when
    Result :: {ok, [filename()]} | error().
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

-spec read_system_migrations(ModuleName :: atom()) -> Result when
    Result :: directory() | error().
read_system_migrations(ModuleName) ->
    case code:lib_dir(ModuleName) of
        {error, Reason} ->
            {error, read_directory_failure, Reason};
        Dir ->
            DatabasePath = database_path(ModuleName, Dir),
            Path = filename:join(DatabasePath, "system"),
            {ok, Path}
    end.

-spec format_bin_content(Bin :: binary()) -> Result when
    Result :: {'ok', sql()} | error().
format_bin_content(Bin) ->
    RemoveLineBreaks = binary:split(Bin, [<<"\n">>], [global]),
    Content = lists:map(fun(X) -> unicode:characters_to_list(X) end, RemoveLineBreaks),
    SQL = lists:concat(Content),
    case string:is_empty(SQL) of
        true ->
            Message = "The provided file is empty",
            {error, empty_sql_file, Message};
        _ ->
            {ok, SQL}
    end.
