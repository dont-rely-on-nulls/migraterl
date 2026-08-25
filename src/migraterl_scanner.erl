-module(migraterl_scanner).
-moduledoc """
Filesystem scanning for migration scripts.

Reads each configured source directory, keeps only `.sql` files,
orders them lexically, and computes a content hash for each. The
result feeds the pure diff engine in `m:migraterl_plan`.
""".

-include("migraterl.hrl").

-export([scan/2, read_system_migrations/0]).

-define(PRIV_DIR_MODULE, migraterl).

-doc """
Scan every configured source, returning the discovered scripts in
application order (sources in the order given; files lexically sorted
within each source).
""".
-spec scan(Namespace, Sources) -> Result when
    Namespace :: namespace(),
    Sources :: [#source{}],
    Result :: {ok, [#script{}]} | {error, term()}.
scan(Namespace, Sources) ->
    scan(Namespace, Sources, []).

scan(_Namespace, [], Acc) ->
    finish_scan(lists:reverse(Acc));
scan(Namespace, [#source{path = Dir, required = Required} = Source | Rest], Acc) ->
    logger:debug("Scanning migration source", source_metadata(Namespace, Source, undefined, Dir)),
    case read_directory(Dir) of
        {ok, Paths} ->
            case build_scripts(Namespace, Source, Paths) of
                {ok, Scripts} ->
                    scan(Namespace, Rest, lists:reverse(Scripts, Acc));
                {error, _} = Err ->
                    Err
            end;
        {error, {list_dir, Dir, enoent}} when Required =:= false ->
            scan(Namespace, Rest, Acc);
        {error, _} = Err ->
            logger:error(
                "Migration source scan failed",
                (source_metadata(Namespace, Source, undefined, Dir))#{failure => list_dir}
            ),
            Err
    end.

build_scripts(Namespace, Source, Paths) ->
    Indexed = lists:zip(lists:seq(0, length(Paths) - 1), Paths),
    try
        Scripts = [build_script(Namespace, Source, Order, Path) || {Order, Path} <- Indexed],
        {ok, Scripts}
    catch
        throw:{read_error, Path, Reason} ->
            logger:error(
                "Migration script scan failed",
                (source_metadata(Namespace, Source, filename:basename(Path), Path))#{
                    failure => read_file
                }
            ),
            {error, {read_error, Path, Reason}}
    end.

build_script(Namespace, #source{id = Id, stage = Stage, class = Class} = Source, Order, Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Name = to_binary_name(filename:basename(Path)),
            logger:debug(
                "Discovered migration script",
                source_metadata(Namespace, Source, Name, Path)
            ),
            #script{
                namespace = Namespace,
                name = Name,
                path = Path,
                class = Class,
                source_id = Id,
                stage = Stage,
                order = Order,
                hash = hash(Bin),
                sql = Bin
            };
        {error, Reason} ->
            throw({read_error, Path, Reason})
    end.

finish_scan(Scripts) ->
    {Groups, Order} = lists:foldl(fun group_script/2, {#{}, []}, Scripts),
    case first_duplicate(lists:reverse(Order), Groups) of
        none ->
            {ok, Scripts};
        {Name, Paths} ->
            log_duplicate(Name, Scripts),
            {error, {duplicate_script_name, Name, Paths}}
    end.

group_script(#script{name = Name, path = Path}, {Groups, Order}) ->
    case maps:find(Name, Groups) of
        error -> {Groups#{Name => [Path]}, [Name | Order]};
        {ok, Paths} -> {Groups#{Name := [Path | Paths]}, Order}
    end.

first_duplicate([], _Groups) ->
    none;
first_duplicate([Name | Rest], Groups) ->
    Paths = maps:get(Name, Groups),
    case Paths of
        [_] -> first_duplicate(Rest, Groups);
        _ -> {Name, lists:reverse(Paths)}
    end.

log_duplicate(Name, Scripts) ->
    #script{
        namespace = Namespace,
        path = Path,
        stage = Stage,
        source_id = SourceId
    } = lists:keyfind(Name, #script.name, Scripts),
    logger:error("Duplicate migration script name", #{
        namespace => Namespace,
        script => Name,
        path => Path,
        stage => Stage,
        source_id => SourceId,
        failure => duplicate_script_name
    }).

source_metadata(Namespace, #source{id = Id, stage = Stage}, Script, Path) ->
    #{
        namespace => Namespace,
        script => script_name(Script),
        path => Path,
        stage => Stage,
        source_id => Id
    }.

script_name(undefined) -> undefined;
script_name(Name) -> to_binary_name(Name).

to_binary_name(Name) when is_binary(Name) -> Name;
to_binary_name(Name) when is_list(Name) -> list_to_binary(Name).

-doc "Lowercase hex sha256 of the given content.".
-spec hash(binary()) -> binary().
hash(Bin) ->
    Digest = crypto:hash(sha256, Bin),
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Digest]).

-doc "List `.sql` files in a directory as absolute, lexically sorted paths.".
-spec read_directory(file:filename_all()) ->
    {ok, [file:filename_all()]} | {error, term()}.
read_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Sql = [
                filename:absname(filename:join(Dir, F))
             || F <- Files, filename:extension(F) =:= ".sql"
            ],
            {ok, lists:sort(Sql)};
        {error, Reason} ->
            {error, {list_dir, Dir, Reason}}
    end.

-doc "Read the bundled system migrations that bootstrap the journal.".
-spec read_system_migrations() -> {ok, [file:filename_all()]} | {error, term()}.
read_system_migrations() ->
    case code:priv_dir(?PRIV_DIR_MODULE) of
        {error, Reason} ->
            {error, Reason};
        Dir ->
            read_directory(filename:join(Dir, "system"))
    end.
