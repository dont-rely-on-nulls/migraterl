-module(migraterl_plan).
-moduledoc """
Pure diff engine.

Given the scripts discovered on disk and the currently-applied journal
state, decides what must run. Has no database or filesystem
dependencies, so it is exhaustively unit-testable in isolation.

Classification rules per script class:

- `once`: run if never applied; if applied but the hash differs, that
  is content drift (a `once` script was edited after the fact) and
  surfaces as a warning, not a re-run.
- `on_change`: run if never applied or if the hash differs.
- `always`: always run; never consulted against the journal.

Out-of-order detection: a not-yet-applied `once` script that sorts
before an already-applied `once` script is a late insertion. Whether
that warns, errors, or is ignored is governed by the out-of-order
policy.
""".

-include("migraterl.hrl").

-export([build/3]).

-doc "Build the plan from disk scripts, current journal entries, and options.".
-spec build(Scripts, Journal, Opts) -> #plan{} when
    Scripts :: [#script{}],
    Journal :: #{binary() => #entry{}},
    Opts :: #opts{}.
build(Scripts, Journal, #opts{on_out_of_order = Policy}) ->
    Plan0 = lists:foldl(fun(S, P) -> classify(S, Journal, P) end, #plan{}, Scripts),
    Plan1 = detect_out_of_order(Scripts, Journal, Policy, Plan0),
    finalize(Plan1).

classify(#script{class = always} = S, _Journal, Plan) ->
    add_action(#action{script = S, reason = always}, Plan);
classify(#script{class = Class, name = Name, hash = Hash} = S, Journal, Plan) ->
    case Journal of
        #{Name := #entry{hash = Hash}} ->
            %% applied and unchanged
            add_skipped(Name, Plan);
        #{Name := #entry{}} when Class =:= on_change ->
            add_action(#action{script = S, reason = changed}, Plan);
        #{Name := #entry{}} when Class =:= once ->
            %% a `once' script whose content changed after application
            add_warning({drift, Name}, Plan);
        _ ->
            add_action(#action{script = S, reason = new}, Plan)
    end.

%% Any `once' script we are about to apply as `new' that sorts before an
%% already-applied `once' script indicates history was inserted into.
detect_out_of_order(_Scripts, _Journal, ignore, Plan) ->
    Plan;
detect_out_of_order(Scripts, Journal, _Policy, Plan) ->
    AppliedOnce = [
        Name
     || #script{class = once, name = Name} <- Scripts, maps:is_key(Name, Journal)
    ],
    case AppliedOnce of
        [] ->
            Plan;
        _ ->
            MaxApplied = lists:max(AppliedOnce),
            NewBefore = [
                Name
             || #action{reason = new, script = #script{class = once, name = Name}} <-
                    Plan#plan.actions,
                Name < MaxApplied
            ],
            lists:foldl(
                fun(Name, P) -> add_warning({out_of_order, Name}, P) end,
                Plan,
                NewBefore
            )
    end.

%% Preserve discovery order for actions and warnings (we prepend then reverse).
finalize(#plan{actions = A, warnings = W, skipped = S} = Plan) ->
    Plan#plan{
        actions = lists:reverse(A),
        warnings = lists:reverse(W),
        skipped = lists:reverse(S)
    }.

add_action(A, #plan{actions = As} = P) -> P#plan{actions = [A | As]}.
add_warning(W, #plan{warnings = Ws} = P) -> P#plan{warnings = [W | Ws]}.
add_skipped(N, #plan{skipped = Ns} = P) -> P#plan{skipped = [N | Ns]}.
