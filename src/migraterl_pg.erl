-module(migraterl_pg).
-moduledoc """
PostgreSQL layer for migraterl.

Thin, explicit wrapper over epgsql. Owns the journal DDL, the version
floor check, session-level advisory locking, the temporal journal
upsert and the post-apply `NOTIFY`. Transaction *boundaries* are driven
by the runner (via `begin_txn/1`, `commit/1`, `rollback/1`) so the same
primitives serve per-script, single and no-transaction modes.
""".

-include("migraterl.hrl").

-export([
    assert_version/1,
    ensure_journal/1,
    advisory_lock/2,
    advisory_unlock/2,
    current_entries/2,
    begin_txn/1,
    commit/1,
    rollback/1,
    apply_one/3
]).

-define(MIN_SERVER_VERSION, 180000).
-define(NOTIFY_CHANNEL, "migraterl_events").

-doc """
Fail fast unless the server is PostgreSQL 18 or newer; the journal
relies on application-time temporal support.
""".
-spec assert_version(epgsql:connection()) -> ok | {error, term()}.
assert_version(Conn) ->
    case epgsql:squery(Conn, "SELECT current_setting('server_version_num')::int") of
        {ok, _, [{Bin}]} ->
            case binary_to_integer(Bin) of
                V when V >= ?MIN_SERVER_VERSION -> ok;
                V -> {error, {unsupported_server_version, V, ?MIN_SERVER_VERSION}}
            end;
        Other ->
            {error, {version_check_failed, Other}}
    end.

-doc "Idempotently create the journal schema from the bundled DDL.".
-spec ensure_journal(epgsql:connection()) -> ok | {error, term()}.
ensure_journal(Conn) ->
    case migraterl_scanner:read_system_migrations() of
        {ok, Files} ->
            run_files(Conn, Files);
        {error, _} = Err ->
            Err
    end.

run_files(_Conn, []) ->
    ok;
run_files(Conn, [Path | Rest]) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case exec_script(Conn, Bin) of
                ok -> run_files(Conn, Rest);
                {error, _} = Err -> Err
            end;
        {error, Reason} ->
            {error, {read_error, Path, Reason}}
    end.

-doc """
Acquire a session-level advisory lock for the namespace. Held across
per-script commits and released in `advisory_unlock/2`.
""".
-spec advisory_lock(epgsql:connection(), namespace()) -> ok | {error, term()}.
advisory_lock(Conn, Namespace) ->
    %% Derive the bigint key in-database via hashtextextended so any external
    %% tooling can compute the identical lock key from the namespace string.
    case epgsql:equery(Conn, "SELECT pg_advisory_lock(hashtextextended($1, 0))", [Namespace]) of
        {ok, _, _} -> ok;
        Other -> {error, {advisory_lock_failed, Other}}
    end.

-doc "Release the namespace advisory lock. Best-effort.".
-spec advisory_unlock(epgsql:connection(), namespace()) -> ok.
advisory_unlock(Conn, Namespace) ->
    _ = epgsql:equery(Conn, "SELECT pg_advisory_unlock(hashtextextended($1, 0))", [Namespace]),
    ok.

-doc "The currently-in-force journal entries for a namespace, keyed by name.".
-spec current_entries(epgsql:connection(), namespace()) ->
    {ok, #{binary() => #entry{}}} | {error, term()}.
current_entries(Conn, Namespace) ->
    Query =
        "SELECT script_name, content_hash, script_class, lower(valid_period)::text "
        "FROM migraterl.schema_journal "
        "WHERE namespace = $1 AND upper(valid_period) IS NULL",
    case epgsql:equery(Conn, Query, [Namespace]) of
        {ok, _Cols, Rows} ->
            {ok,
                maps:from_list([{Name, to_entry(Namespace, Row)} || {Name, _, _, _} = Row <- Rows])};
        Other ->
            {error, {current_entries_failed, Other}}
    end.

to_entry(Namespace, {Name, Hash, Class, AppliedAt}) ->
    #entry{
        namespace = Namespace,
        name = Name,
        hash = Hash,
        class = binary_to_existing_atom(Class, utf8),
        applied_at = AppliedAt
    }.

-spec begin_txn(epgsql:connection()) -> ok | {error, term()}.
begin_txn(Conn) -> simple(Conn, "BEGIN").

-spec commit(epgsql:connection()) -> ok | {error, term()}.
commit(Conn) -> simple(Conn, "COMMIT").

-spec rollback(epgsql:connection()) -> ok | {error, term()}.
rollback(Conn) -> simple(Conn, "ROLLBACK").

simple(Conn, Stmt) ->
    case epgsql:squery(Conn, Stmt) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        Other -> {error, {Stmt, Other}}
    end.

-doc """
Execute one script and record it in the journal. Does NOT manage
transaction boundaries; the caller wraps this as its txn mode dictates.
Returns the execution time in milliseconds.
""".
-spec apply_one(epgsql:connection(), #action{}, #opts{}) ->
    {ok, non_neg_integer()} | {error, term()}.
apply_one(Conn, #action{script = Script, reason = Reason}, Opts) ->
    #script{name = Name, sql = Raw} = Script,
    SQL = substitute(Raw, Opts#opts.variables),
    T0 = erlang:monotonic_time(millisecond),
    case exec_script(Conn, SQL) of
        ok ->
            Ms = erlang:monotonic_time(millisecond) - T0,
            case record(Conn, Script, Reason, Ms) of
                ok ->
                    ok = maybe_notify(Conn, Script, Reason, Opts),
                    {ok, Ms};
                {error, _} = Err ->
                    Err
            end;
        {error, Reason1} ->
            {error, {script_error, Name, Reason1}}
    end.

%% `always' scripts are never journaled; they simply run every time.
record(_Conn, #script{class = always}, _Reason, _Ms) ->
    ok;
record(Conn, #script{namespace = Ns, name = Name, hash = Hash, class = Class}, new, Ms) ->
    Insert =
        "INSERT INTO migraterl.schema_journal "
        "(namespace, script_name, content_hash, script_class, execution_ms, valid_period) "
        "VALUES ($1, $2, $3, $4, $5, tstzrange(current_timestamp, NULL))",
    upsert(Conn, Insert, [Ns, Name, Hash, atom_to_binary(Class, utf8), Ms]);
record(Conn, #script{namespace = Ns, name = Name, hash = Hash, class = Class}, changed, Ms) ->
    %% Close the currently-valid period at now(), then open a fresh one.
    %% The WITHOUT OVERLAPS temporal primary key guarantees the two rows
    %% never overlap, preserving a full audit trail with no triggers.
    Close =
        "UPDATE migraterl.schema_journal "
        "SET valid_period = tstzrange(lower(valid_period), current_timestamp) "
        "WHERE namespace = $1 AND script_name = $2 AND upper(valid_period) IS NULL",
    Insert =
        "INSERT INTO migraterl.schema_journal "
        "(namespace, script_name, content_hash, script_class, execution_ms, valid_period) "
        "VALUES ($1, $2, $3, $4, $5, tstzrange(current_timestamp, NULL))",
    case upsert(Conn, Close, [Ns, Name]) of
        ok -> upsert(Conn, Insert, [Ns, Name, Hash, atom_to_binary(Class, utf8), Ms]);
        {error, _} = Err -> Err
    end.

upsert(Conn, Query, Params) ->
    case epgsql:equery(Conn, Query, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {ok, _, _, _} -> ok;
        Other -> {error, {journal_write_failed, Other}}
    end.

maybe_notify(_Conn, _Script, _Reason, #opts{notify = false}) ->
    ok;
maybe_notify(Conn, #script{namespace = Ns, name = Name, hash = Hash}, Reason, _Opts) ->
    Payload = json_event(Ns, Name, Hash, Reason),
    case epgsql:equery(Conn, "SELECT pg_notify($1, $2)", [?NOTIFY_CHANNEL, Payload]) of
        {ok, _, _} -> ok;
        %% NOTIFY is a best-effort hint; the journal is the source of truth.
        _ -> ok
    end.

%% ---------------
%% Helpers
%% ---------------

%% Run a (possibly multi-statement) SQL script via the simple query
%% protocol and collapse the results into ok | {error, Reason}.
-spec exec_script(epgsql:connection(), binary()) -> ok | {error, term()}.
exec_script(Conn, SQL) ->
    Results = normalize(epgsql:squery(Conn, SQL)),
    case lists:keyfind(error, 1, Results) of
        false -> ok;
        {error, Reason} -> {error, Reason}
    end.

normalize(Result) when is_list(Result) -> Result;
normalize(Result) -> [Result].

%% Substitute $key$ tokens with their values, longest keys first so that
%% overlapping names cannot partially match.
substitute(SQL, Variables) when map_size(Variables) =:= 0 ->
    SQL;
substitute(SQL, Variables) ->
    Pairs = lists:sort(
        fun({A, _}, {B, _}) -> byte_size(A) >= byte_size(B) end,
        maps:to_list(Variables)
    ),
    lists:foldl(
        fun({Key, Value}, Acc) ->
            binary:replace(Acc, <<"$", Key/binary, "$">>, Value, [global])
        end,
        SQL,
        Pairs
    ).

json_event(Ns, Name, Hash, Reason) ->
    Fields = [
        {<<"ns">>, Ns},
        {<<"script">>, Name},
        {<<"hash">>, Hash},
        {<<"reason">>, atom_to_binary(Reason, utf8)}
    ],
    Body = lists:join(<<",">>, [[<<"\"">>, K, <<"\":\"">>, esc(V), <<"\"">>] || {K, V} <- Fields]),
    iolist_to_binary([<<"{">>, Body, <<"}">>]).

esc(Bin) ->
    binary:replace(binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]), <<"\"">>, <<"\\\"">>, [
        global
    ]).
