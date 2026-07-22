-ifndef(MIGRATERL_HRL).
-define(MIGRATERL_HRL, true).

%% ---------------
%% Core Types
%% ---------------

%% A logical grouping of scripts tracked independently in the journal.
-type namespace() :: binary().

%% How a script participates in the run:
%%   once      - applied a single time, keyed by name
%%   on_change - re-applied whenever its content hash changes
%%   always    - applied on every run, never journaled
-type script_class() :: once | on_change | always.

%% Transaction granularity for a run.
-type txn_mode() :: per_script | single | none.

%% What to do when a not-yet-applied `once' script sorts *before* one that has
%% already been applied (a late insertion into history).
-type ooo_policy() :: warn | error | ignore.

%% Why a script ended up in the plan.
-type action_reason() :: new | changed | always.

%% Non-fatal (or policy-gated) observations from the diff engine.
-type warning() ::
    {out_of_order, binary()}
    | {drift, binary()}
    | {missing, binary()}.

%% ---------------
%% Records
%% ---------------

%% A script discovered on disk during the scanning phase.
-record(script, {
    namespace :: namespace(),
    name :: binary(),
    path :: file:filename_all(),
    class :: script_class(),
    %% lexical position within its own source directory (0-based)
    order :: non_neg_integer(),
    %% lowercase hex sha256 of the raw file contents (pre-substitution)
    hash :: binary(),
    %% raw file contents; variable substitution happens at apply time
    sql :: binary()
}).

%% A currently-in-force journal row (upper(valid_period) IS NULL).
-record(entry, {
    namespace :: namespace(),
    name :: binary(),
    hash :: binary(),
    class :: once | on_change,
    applied_at :: binary()
}).

%% A single planned application.
-record(action, {
    script :: #script{},
    reason :: action_reason()
}).

%% The output of the pure diff engine.
-record(plan, {
    actions = [] :: [#action{}],
    warnings = [] :: [warning()],
    %% names skipped because already applied and unchanged
    skipped = [] :: [binary()]
}).

%% Options for a run. All fields optional; see migraterl:default_opts/0.
-record(opts, {
    namespace = <<"default">> :: namespace(),
    %% ordered list of {Class, Directory}
    sources = [] :: [{script_class(), file:filename_all()}],
    txn = per_script :: txn_mode(),
    on_out_of_order = warn :: ooo_policy(),
    %% $key$ -> value substitution applied to SQL at execution time
    variables = #{} :: #{binary() => binary()},
    %% stop after planning; report the plan without applying
    dry_run = false :: boolean(),
    %% emit pg_notify('migraterl_events', ...) after each apply
    notify = true :: boolean()
}).

-endif.
