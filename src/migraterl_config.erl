-module(migraterl_config).
-moduledoc "Internal normalization of public Migraterl run options.".

-include("migraterl.hrl").

-export([normalize/1, watch_dirs/1]).

-define(ALLOWED_KEYS, [
    namespace, sources, layout, txn, on_out_of_order, variables, dry_run, notify
]).

-doc "Validate lifecycle options and normalize every source to `#source{}`.".
-spec normalize(map()) -> {ok, #opts{}} | {error, {invalid_options, term()}}.
normalize(Map) when is_map(Map) ->
    case unknown_keys(Map, ?ALLOWED_KEYS) of
        [] -> normalize_known(Map);
        Unknown -> invalid({unknown_keys, Unknown})
    end;
normalize(Other) ->
    invalid({options_not_map, Other}).

-doc "Directories a watcher should observe for normalized options.".
-spec watch_dirs(#opts{}) -> [file:filename_all()].
watch_dirs(#opts{watch_dirs = Dirs}) -> Dirs.

normalize_known(Map) ->
    Specs = [
        {namespace, <<"default">>, fun normalize_namespace/1},
        {txn, per_script, fun normalize_txn/1},
        {on_out_of_order, warn, fun normalize_out_of_order/1},
        {variables, #{}, fun normalize_variables/1},
        {dry_run, false, fun normalize_dry_run/1},
        {notify, true, fun normalize_notify/1}
    ],
    case normalize_values(Specs, Map, #{}) of
        {ok, Values} ->
            case normalize_sources(Map) of
                {ok, Sources, WatchDirs} ->
                    {ok, #opts{
                        namespace = maps:get(namespace, Values),
                        sources = Sources,
                        watch_dirs = WatchDirs,
                        txn = maps:get(txn, Values),
                        on_out_of_order = maps:get(on_out_of_order, Values),
                        variables = maps:get(variables, Values),
                        dry_run = maps:get(dry_run, Values),
                        notify = maps:get(notify, Values)
                    }};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

normalize_values([], _Map, Acc) ->
    {ok, Acc};
normalize_values([{Key, Default, Normalize} | Rest], Map, Acc) ->
    Value = maps:get(Key, Map, Default),
    case Normalize(Value) of
        {ok, Normalized} -> normalize_values(Rest, Map, Acc#{Key => Normalized});
        {error, Reason} -> invalid(Reason)
    end.

normalize_sources(Map) ->
    case {maps:is_key(sources, Map), maps:is_key(layout, Map)} of
        {true, true} ->
            invalid(conflicting_sources_and_layout);
        {false, true} ->
            case migraterl_layout:expand(maps:get(layout, Map)) of
                {ok, Sources, Root} -> {ok, Sources, [Root]};
                {error, Reason} -> invalid(Reason)
            end;
        {true, false} ->
            normalize_legacy(maps:get(sources, Map));
        {false, false} ->
            {ok, [], []}
    end.

normalize_legacy(Sources) when is_list(Sources) ->
    normalize_legacy(Sources, 0, [], []);
normalize_legacy(Other) ->
    invalid({invalid_sources, Other}).

normalize_legacy([], _Index, SourcesAcc, DirsAcc) ->
    {ok, lists:reverse(SourcesAcc), lists:usort(DirsAcc)};
normalize_legacy([{Class, Dir} | Rest], Index, SourcesAcc, DirsAcc) ->
    case valid_class(Class) andalso valid_filename(Dir) of
        true ->
            Id = iolist_to_binary(["legacy_", integer_to_list(Index)]),
            Source = #source{id = Id, stage = legacy, path = Dir, class = Class, required = true},
            normalize_legacy(Rest, Index + 1, [Source | SourcesAcc], [
                filename:absname(Dir) | DirsAcc
            ]);
        false ->
            invalid({invalid_source, Index, {Class, Dir}})
    end;
normalize_legacy([Source | _], Index, _SourcesAcc, _DirsAcc) ->
    invalid({invalid_source, Index, Source});
normalize_legacy(ImproperTail, Index, _SourcesAcc, _DirsAcc) ->
    invalid({invalid_source, Index, ImproperTail}).

normalize_namespace(Value) ->
    case namespace_binary(Value) of
        {ok, Bin} ->
            case unicode:characters_to_list(Bin) of
                Chars when is_list(Chars), Chars =/= [], length(Chars) =< 255 ->
                    case binary:match(Bin, <<0>>) of
                        nomatch -> {ok, Bin};
                        _ -> {error, {invalid_namespace, Value}}
                    end;
                _ ->
                    {error, {invalid_namespace, Value}}
            end;
        error ->
            {error, {invalid_namespace, Value}}
    end.

namespace_binary(Bin) when is_binary(Bin) ->
    {ok, Bin};
namespace_binary(Atom) when is_atom(Atom) ->
    {ok, atom_to_binary(Atom, utf8)};
namespace_binary(List) when is_list(List) ->
    case flat_char_list(List) of
        true ->
            try unicode:characters_to_binary(List) of
                Bin when is_binary(Bin) -> {ok, Bin};
                _ -> error
            catch
                error:badarg -> error
            end;
        false ->
            error
    end;
namespace_binary(_) ->
    error.

normalize_txn(per_script) -> {ok, per_script};
normalize_txn(single) -> {ok, single};
normalize_txn(none) -> {ok, none};
normalize_txn(Value) -> {error, {invalid_txn, Value}}.

normalize_out_of_order(warn) -> {ok, warn};
normalize_out_of_order(error) -> {ok, error};
normalize_out_of_order(ignore) -> {ok, ignore};
normalize_out_of_order(Value) -> {error, {invalid_on_out_of_order, Value}}.

normalize_variables(Variables) when is_map(Variables) ->
    case
        lists:all(
            fun({Key, Value}) -> is_binary(Key) andalso is_binary(Value) end,
            maps:to_list(Variables)
        )
    of
        true -> {ok, Variables};
        false -> {error, {invalid_variables, Variables}}
    end;
normalize_variables(Value) ->
    {error, {invalid_variables, Value}}.

normalize_dry_run(Value) when is_boolean(Value) -> {ok, Value};
normalize_dry_run(Value) -> {error, {invalid_dry_run, Value}}.

normalize_notify(Value) when is_boolean(Value) -> {ok, Value};
normalize_notify(Value) -> {error, {invalid_notify, Value}}.

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

unknown_keys(Map, Allowed) ->
    lists:sort(maps:keys(Map) -- Allowed).

invalid(Reason) ->
    {error, {invalid_options, Reason}}.
