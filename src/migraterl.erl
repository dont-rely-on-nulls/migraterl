-module(migraterl).
-moduledoc """
migraterl public API.

A forward-only SQL migration engine for PostgreSQL 18+. Scripts are
grouped into a namespace and classified as `once`, `on_change` or
`always`; the engine diffs the directories against the temporal
journal and applies only what is needed, one namespace-locked run at
a time.

## Example

```erlang
Conn = migraterl:default_connection(),
{ok, Summary} = migraterl:migrate(Conn, #{
    namespace => <<"app">>,
    sources => [
        {once,      "priv/migrations/schema"},
        {on_change, "priv/migrations/views"},
        {always,    "priv/migrations/grants"}
    ],
    txn => per_script,
    variables => #{<<"env">> => <<"prod">>}
}).
```
""".

-include("migraterl.hrl").

-export([default_connection/0, migrate/2, plan/2, status/2]).

-define(DEFAULT_TIMEOUT, 60000).

-type summary() :: #{
    planned := [binary()],
    applied := [binary()],
    skipped := [binary()],
    warnings := [warning()]
}.
-export_type([summary/0]).

-doc "A default local/CI connection driven by `PG*` environment variables.".
-spec default_connection() -> epgsql:connection() | {error, term()}.
default_connection() ->
    Config = #{
        host => os:getenv("PGHOST", "127.0.0.1"),
        port => list_to_integer(os:getenv("PGPORT", "5432")),
        username => os:getenv("PGUSER", "migraterl"),
        password => os:getenv("PGPASSWORD", "migraterl"),
        database => os:getenv("PGDATABASE", "migraterl"),
        timeout => 10000
    },
    case epgsql:connect(Config) of
        {ok, Conn} -> Conn;
        Otherwise -> {error, {connect_failed, Otherwise}}
    end.

-doc "Apply all pending migrations for the given configuration.".
-spec migrate(epgsql:connection(), map()) -> {ok, summary()} | {error, term()}.
migrate(Conn, OptsMap) ->
    migraterl_runner:run(Conn, opts(OptsMap), ?DEFAULT_TIMEOUT).

-doc """
Dry run: compute and return what would be applied without touching any
target schema (the journal is still created and read).
""".
-spec plan(epgsql:connection(), map()) -> {ok, summary()} | {error, term()}.
plan(Conn, OptsMap) ->
    Opts = (opts(OptsMap))#opts{dry_run = true},
    migraterl_runner:run(Conn, Opts, ?DEFAULT_TIMEOUT).

-doc "The currently-applied journal state for a namespace.".
-spec status(epgsql:connection(), namespace()) -> {ok, [map()]} | {error, term()}.
status(Conn, Namespace) ->
    case migraterl_pg:current_entries(Conn, Namespace) of
        {ok, Entries} ->
            {ok, [
                #{
                    name => E#entry.name,
                    hash => E#entry.hash,
                    class => E#entry.class,
                    applied_at => E#entry.applied_at
                }
             || E <- maps:values(Entries)
            ]};
        {error, _} = Err ->
            Err
    end.

%% ---------------
%% Options
%% ---------------

opts(Map) ->
    #opts{
        namespace = to_bin(maps:get(namespace, Map, <<"default">>)),
        sources = maps:get(sources, Map, []),
        txn = maps:get(txn, Map, per_script),
        on_out_of_order = maps:get(on_out_of_order, Map, warn),
        variables = maps:get(variables, Map, #{}),
        dry_run = maps:get(dry_run, Map, false),
        notify = maps:get(notify, Map, true)
    }.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).
