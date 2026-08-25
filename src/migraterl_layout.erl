-module(migraterl_layout).
-moduledoc """
Expansion and validation for staged migration layouts.

A layout is either the built-in `grate` profile or an explicitly ordered
list of stage maps. Layout stages are normalized to the same source record
used by the legacy `{Class, Directory}` API.
""".

-include("migraterl.hrl").

-export([expand/1]).

-type expand_error() ::
    {layout_not_map, term()}
    | missing_root
    | {unknown_keys, [term()]}
    | conflicting_profile_and_stages
    | missing_profile_or_stages
    | {unknown_profile, term()}
    | {invalid_root, term()}
    | {invalid_stages, term()}
    | {invalid_stage, non_neg_integer(), term()}
    | {duplicate_stage_id, source_id()}.

-doc "Expand a lifecycle layout into ordered sources and its watch root.".
-spec expand(map()) ->
    {ok, [#source{}], file:filename_all()} | {error, {invalid_layout, expand_error()}}.
expand(Layout) when is_map(Layout) ->
    case unknown_keys(Layout, [root, profile, stages]) of
        [] ->
            expand_known(Layout);
        Unknown ->
            invalid({unknown_keys, Unknown})
    end;
expand(Other) ->
    invalid({layout_not_map, Other}).

expand_known(Layout) ->
    case maps:find(root, Layout) of
        error ->
            invalid(missing_root);
        {ok, Root} ->
            case valid_filename(Root) of
                true -> expand_kind(Layout, Root);
                false -> invalid({invalid_root, Root})
            end
    end.

expand_kind(#{profile := _, stages := _}, _Root) ->
    invalid(conflicting_profile_and_stages);
expand_kind(#{profile := grate}, Root) ->
    {ok, profile_sources(Root), filename:absname(Root)};
expand_kind(#{profile := Profile}, _Root) ->
    invalid({unknown_profile, Profile});
expand_kind(#{stages := Stages}, Root) when is_list(Stages) ->
    case expand_stages(Stages, Root, 0, #{}, []) of
        {ok, Sources} -> {ok, Sources, filename:absname(Root)};
        {error, _} = Err -> Err
    end;
expand_kind(#{stages := Stages}, _Root) ->
    invalid({invalid_stages, Stages});
expand_kind(_Layout, _Root) ->
    invalid(missing_profile_or_stages).

profile_sources(Root) ->
    [
        source(before_migration, Root, "beforeMigration", always, false),
        source(alter_database, Root, "alterDatabase", on_change, false),
        source(before_once, Root, "runBeforeUp", on_change, false),
        source(linear, Root, "up", once, false),
        source(after_once, Root, "runFirstAfterUp", once, false),
        source(functions, Root, "functions", on_change, false),
        source(views, Root, "views", on_change, false),
        source(procedures, Root, "sprocs", on_change, false),
        source(triggers, Root, "triggers", on_change, false),
        source(indexes, Root, "indexes", on_change, false),
        source(after_change, Root, "runAfterOtherAnyTimeScripts", on_change, false),
        source(permissions, Root, "permissions", always, false),
        source(after_migration, Root, "afterMigration", always, false)
    ].

expand_stages([], _Root, _Index, _Ids, Acc) ->
    {ok, lists:reverse(Acc)};
expand_stages([Stage | Rest], Root, Index, Ids, Acc) when is_map(Stage) ->
    case expand_stage(Stage, Root, Index) of
        {ok, #source{id = Id} = Source} ->
            case maps:is_key(Id, Ids) of
                true -> invalid({duplicate_stage_id, Id});
                false -> expand_stages(Rest, Root, Index + 1, Ids#{Id => true}, [Source | Acc])
            end;
        {error, _} = Err ->
            Err
    end;
expand_stages([Stage | _], _Root, Index, _Ids, _Acc) ->
    invalid({invalid_stage, Index, Stage});
expand_stages(ImproperTail, _Root, _Index, _Ids, _Acc) ->
    invalid({invalid_stages, ImproperTail}).

expand_stage(Stage, Root, Index) ->
    case unknown_keys(Stage, [id, stage, path, class, required]) of
        [] -> validate_stage(Stage, Root, Index);
        Unknown -> invalid({invalid_stage, Index, {unknown_keys, Unknown}})
    end.

validate_stage(Stage, Root, Index) ->
    Id = maps:get(id, Stage, undefined),
    StageName = maps:get(stage, Stage, undefined),
    Path = maps:get(path, Stage, undefined),
    Class = maps:get(class, Stage, undefined),
    Required = maps:get(required, Stage, true),
    case
        {
            valid_id(Id),
            valid_stage(StageName),
            valid_relative_path(Path),
            valid_class(Class),
            is_boolean(Required)
        }
    of
        {true, true, true, true, true} ->
            {ok, source(Id, StageName, Root, Path, Class, Required)};
        _ ->
            invalid({invalid_stage, Index, Stage})
    end.

source(Id, Root, Path, Class, Required) ->
    source(Id, Id, Root, Path, Class, Required).

source(Id, Stage, Root, Path, Class, Required) ->
    #source{
        id = Id,
        stage = Stage,
        path = filename:join(Root, Path),
        class = Class,
        required = Required
    }.

valid_id(undefined) -> false;
valid_id('') -> false;
valid_id(Id) when is_atom(Id) -> true;
valid_id(Id) when is_binary(Id) -> byte_size(Id) > 0;
valid_id(_) -> false.

valid_stage(undefined) -> false;
valid_stage('') -> false;
valid_stage(Stage) when is_atom(Stage) -> true;
valid_stage(Stage) when is_binary(Stage) -> byte_size(Stage) > 0;
valid_stage(_) -> false.

valid_class(once) -> true;
valid_class(on_change) -> true;
valid_class(always) -> true;
valid_class(_) -> false.

valid_filename(Path) when is_binary(Path) ->
    byte_size(Path) > 0 andalso binary:match(Path, <<0>>) =:= nomatch;
valid_filename(Path) when is_list(Path) ->
    Path =/= [] andalso valid_filename_chars(Path);
valid_filename(_) ->
    false.

valid_filename_chars(Path) ->
    case flat_char_list(Path) of
        true ->
            try unicode:characters_to_binary(Path) of
                Bin when is_binary(Bin) -> binary:match(Bin, <<0>>) =:= nomatch;
                _ -> false
            catch
                error:badarg -> false
            end;
        false ->
            false
    end.

flat_char_list([]) -> true;
flat_char_list([Char | Rest]) when is_integer(Char) -> flat_char_list(Rest);
flat_char_list(_) -> false.

valid_relative_path(Path) ->
    valid_filename(Path) andalso
        filename:pathtype(Path) =:= relative andalso
        not lists:any(fun parent_segment/1, filename:split(Path)).

parent_segment(<<"..">>) -> true;
parent_segment("..") -> true;
parent_segment(_) -> false.

unknown_keys(Map, Allowed) ->
    lists:sort(maps:keys(Map) -- Allowed).

invalid(Reason) ->
    {error, {invalid_layout, Reason}}.
