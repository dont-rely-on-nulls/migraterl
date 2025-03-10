%%-------------------------------------------------------------------
% @doc Internal module, handy functions for file and directory
%      manipulation.
% @end
%%-------------------------------------------------------------------
-module(file_utils).

-export([read_system_migrations/1, format_bin_content/1, read_directory/1]).

-include("internal_types.hrl").

-spec profile_path(Dir :: directory()) -> directory().
profile_path(Dir) ->
    filename:dirname(
        filename:dirname(Dir)
    ).

-spec system_path(ModuleName :: atom(), Dir :: directory()) -> directory().
system_path(ModuleName, Dir) ->
    SystemPath = ["rel", ModuleName, "database", "system"],
    Release = profile_path(Dir),
    filename:join([Release | SystemPath]).

-spec read_directory(Dir :: directory()) -> Result when
    Result :: {ok, [filename()]} | error().
read_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            List = lists:map(fun(File) -> filename:join(Dir, File) end, Files),
            Sorted = lists:sort(List),
            {ok, Sorted};
        {error, Reason} ->
            {error, read_directory_failure, Reason}
    end.

-spec read_system_migrations(ModuleName :: atom()) -> Result when
    Result :: [filename()] | error().
read_system_migrations(ModuleName) ->
    case code:lib_dir(ModuleName) of
        {error, Reason} ->
            {error, read_directory_failure, Reason};
        Dir ->
            Path = system_path(ModuleName, Dir),
            {ok, Path}
    end.

-spec format_bin_content(Bin :: binary()) -> [sql()].
format_bin_content(Bin) ->
    RemoveLineBreaks = binary:split(Bin, [<<"\n">>], [global]),
    Content = lists:map(fun(X) -> unicode:characters_to_list(X) end, RemoveLineBreaks),
    lists:concat(Content).
