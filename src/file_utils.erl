%%-------------------------------------------------------------------
% @doc Internal module, handy functions for file and directory
%      manipulation.
% @end
%%-------------------------------------------------------------------
-module(file_utils).

-export([read_system_migrations/0, read_directory/1]).
-compile({parse_transform, do}).

-include("file_utils.hrl").

-define(PRIV_DIR_MODULE, migraterl).

-spec read_directory(Dir :: directory()) -> Result when
    Reason :: string(),
    Error :: {error, Reason},
    Ok :: {ok, [filename()]},
    Result :: Ok | Error.
read_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} when is_list(Files) ->
            List = lists:map(fun(File) -> filename:absname(filename:join([Dir, File])) end, Files),
            Predicate = fun(File) -> filename:extension(File) =:= ".sql" end,
            Filter = lists:filter(Predicate, List),
            Sorted = lists:sort(Filter),
            {ok, Sorted};
        _ ->
            Reason = io_lib:format("Error while reading directory ~p~n", [Dir]),
            {error, Reason}
    end.

-spec read_system_migrations() -> Result when
    Reason :: string(),
    Error :: {error, Reason},
    Ok :: {ok, [filename()]},
    Result :: Ok | Error.
read_system_migrations() ->
    case code:priv_dir(?PRIV_DIR_MODULE) of
        {error, Reason} ->
            {error, Reason};
        Dir ->
            do([
                error_m
             || Path = filename:join(Dir, "system"),
                Files <- read_directory(Path),
                return(Files)
            ])
    end.
