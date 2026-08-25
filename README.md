# Migraterl

[![Built with Nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
[![[Nix] Build](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/ci.yml/badge.svg)](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/ci.yml)
![Hex.pm Version](https://img.shields.io/hexpm/v/migraterl)
![License](https://img.shields.io/github/license/dont-rely-on-nulls/migraterl)

A SQL migration engine for **PostgreSQL**, built on `gen_statem` and a temporal tables, hash-based journal.

## Status

> [!NOTE]
> This is still experimental, expect heavy API chages before 1.0.0.

### Requirements

- **PostgreSQL 18+**: the journal relies on application-time temporal support (`WITHOUT OVERLAPS` primary keys and `UPDATE ... FOR PORTION OF`). The trusted `btree_gist` extension is created automatically on first run.

### How it works

A run is modelled as a `gen_statem` that walks a linear lifecycle:

```mermaid
stateDiagram-v2
    direction LR
    [*] --> idle
    idle --> ensuring_journal
    ensuring_journal --> locking
    locking --> reading_state
    reading_state --> scanning
    scanning --> planning
    planning --> applying
    applying --> done
    done --> [*]
```

- **Namespaces** group scripts that are journaled independently.
- Each script is classified by the source it comes from:
  - `once`: applied a single time, keyed by name.
  - `on_change`: re-applied whenever its content hash changes. Its SQL must be idempotent or replace the object it owns.
  - `always`: applied on every run, never journaled. Its SQL must be safe on every invocation.
- A **pure diff engine** compares the on-disk SHA-256 hashes against the journal and applies only what is needed, detecting content drift and out-of-order insertions.
- Every run takes a **session advisory lock** on the namespace, so concurrent deploys serialize safely.
- Applied scripts are recorded in a **temporal journal**, re-applying an `on_change` script closes the previous validity period and opens a new one, preserving a full audit trail with no triggers.

### Usage

```erlang
Conn = migraterl:default_connection(),

{ok, Summary} = migraterl:migrate(Conn, #{
    namespace => <<"app">>,
    sources => [
        {once,      "priv/migrations/schema"},
        {on_change, "priv/migrations/views"},
        {always,    "priv/migrations/grants"}
    ],
    txn => per_script,                 % per_script | single | none
    on_out_of_order => warn,           % warn | error | ignore
    variables => #{<<"env">> => <<"prod">>}
}),
%% Summary :: #{planned := [...], applied := [...], skipped := [...], warnings := [...]}

%% Dry run, compute the plan without applying:
{ok, Plan} = migraterl:plan(Conn, Opts),

%% Inspect the currently-applied state:
{ok, State} = migraterl:status(Conn, <<"app">>).
```

Variables written as `$name$` in a script are substituted at execution time.

### Lifecycle layouts (opt-in)

For projects that prefer a conventional directory lifecycle, Migraterl can
expand a Grate-inspired profile into the existing `once`, `on_change`, and
`always` classes:

```erlang
{ok, Summary} = migraterl:migrate(Conn, #{
    namespace => <<"app">>,
    layout => #{
        profile => grate,
        root => "priv/migrations"
    }
}).
```

The profile checks the following optional directories in order:

| Stage | Directory | Class |
| --- | --- | --- |
| `before_migration` | `beforeMigration` | `always` |
| `alter_database` | `alterDatabase` | `on_change` |
| `before_once` | `runBeforeUp` | `on_change` |
| `linear` | `up` | `once` |
| `after_once` | `runFirstAfterUp` | `once` |
| `functions` | `functions` | `on_change` |
| `views` | `views` | `on_change` |
| `procedures` | `sprocs` | `on_change` |
| `triggers` | `triggers` | `on_change` |
| `indexes` | `indexes` | `on_change` |
| `after_change` | `runAfterOtherAnyTimeScripts` | `on_change` |
| `permissions` | `permissions` | `always` |
| `after_migration` | `afterMigration` | `always` |

Layouts can also define a fully custom ordered lifecycle. Custom directories are required by default, set `required => false` to skip a missing directory.

```erlang
#{
    namespace => <<"app">>,
    layout => #{
        root => "priv/migrations",
        stages => [
            #{id => prepare_source, stage => prepare,
              path => "prepare", class => always},
            #{id => linear_source, stage => linear,
              path => "up", class => once},
            #{id => functions_source, stage => replaceable,
              path => "functions",
              class => on_change, required => false}
        ]
    }
}
```

Stage list order takes precedence across directories, files remain lexically ordered within each directory. `sources` and `layout` are **mutually exclusive**, and stage paths must be relative to the layout root. Because the journal keeps its backward-compatible `{namespace, basename}` identity, duplicate SQL basenames across directories are rejected before user scripts execute.

Every custom source requires both a stable `id` and a lifecycle `stage`. Source IDs must be unique within the layout; stage names may repeat. IDs and stages are diagnostic metadata and do not alter the backward-compatible journal identity.

### Option validation

Migration options are fully validated before Migraterl uses the database connection. Unknown top-level keys, invalid enum or boolean values, malformed paths, invalid layouts, duplicate source IDs, and non-binary variable keys or values return `{error, {invalid_options, Reason}}`. Layout-specific reasons retain the `{invalid_layout, Reason}` envelope.

The supported script classes remain `once`, `on_change`, and `always`. The conceptual name `repeat` is not accepted by the v0.5 API; use `always` for scripts that must execute on every invocation.

### Reactive Extras (opt-in)

- `migraterl_listener`: holds a dedicated connection that `LISTEN`s on `migraterl_events` and forwards `pg_notify` hints (emitted on each apply) to subscribed processes.
- `migraterl_watcher`: a supervised `gen_statem` that uses the optional [`fs`](https://github.com/synrc/fs) application to watch source directories and re-run (debounced) on `.sql` changes; handy for "apply on save" during local development.

Configure watchers in the application environment and `migraterl_sup` keeps one alive per entry, so the app auto-migrates on change without any glue code:

```erlang
{migraterl, [
    {watchers, [
        #{conn => #{host => "127.0.0.1", username => "app",
                    password => "app", database => "app"},
          debounce_ms => 300,
          migrate => #{namespace => <<"app">>,
                       sources => [{once, "priv/migrations/schema"}]}}
    ]}
]}.
```

With no `watchers` configured the supervisor runs empty and each `migraterl:migrate/2` call spawns its own transient runner.

## Development

We have [devenv](https://devenv.sh/) setup and everything is based on [Nix](https://nixos.org/), you can check our [flake.nix](https://github.com/dont-rely-on-nulls/migraterl/blob/master/flake.nix) to learn how it looks like.

```shell
nix develop --impure
# to spawn a postgres database
devenv up
```
there's also a [justfile](https://github.com/casey/just) to manage builds and tests.
```shell
# will show all commands supported
just
```

### Testing

```shell
# You can either run rebar directly
rebar3 ct
# or
just t
```

## Inspiration

- [DbUp](https://dbup.readthedocs.io/en/latest/)
- [Grate](https://grate-devs.github.io/grate/)
