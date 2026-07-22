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
    Sources :: [{script_class(), file:filename_all()}],
    Result :: {ok, [#script{}]} | {error, term()}.
scan(Namespace, Sources) ->
    scan(Namespace, Sources, []).

scan(_Namespace, [], Acc) ->
    {ok, lists:reverse(Acc)};
scan(Namespace, [{Class, Dir} | Rest], Acc) ->
    case read_directory(Dir) of
        {ok, Paths} ->
            case build_scripts(Namespace, Class, Paths) of
                {ok, Scripts} ->
                    scan(Namespace, Rest, lists:reverse(Scripts, Acc));
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

build_scripts(Namespace, Class, Paths) ->
    Indexed = lists:zip(lists:seq(0, length(Paths) - 1), Paths),
    try
        Scripts = [build_script(Namespace, Class, Order, Path) || {Order, Path} <- Indexed],
        {ok, Scripts}
    catch
        throw:{read_error, Path, Reason} ->
            {error, {read_error, Path, Reason}}
    end.

build_script(Namespace, Class, Order, Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            #script{
                namespace = Namespace,
                name = list_to_binary(filename:basename(Path)),
                path = Path,
                class = Class,
                order = Order,
                hash = hash(Bin),
                sql = Bin
            };
        {error, Reason} ->
            throw({read_error, Path, Reason})
    end.

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
