-module(migraterl_sup).
-moduledoc """
Top-level supervisor.

Starts one long-lived `m:migraterl_watcher` per entry in the `watchers`
application environment, so a host application can enable "auto-migrate
on change" purely through configuration:

```erlang
{migraterl, [
    {watchers, [
        #{conn => #{host => "127.0.0.1", username => "app"},
          debounce_ms => 300,
          migrate => #{namespace => <<"app">>,
                       sources => [{once, "priv/migrations/schema"}]}}
    ]}
]}
```

With no `watchers` configured the supervisor runs empty: each migration
then spawns its own transient runner (see `m:migraterl_runner`) and
nothing is kept alive.
""".
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, watcher_children()}}.

watcher_children() ->
    Specs = application:get_env(migraterl, watchers, []),
    [watcher_child(N, Spec) || {N, Spec} <- lists:enumerate(Specs)].

watcher_child(N, Spec) ->
    #{
        id => {migraterl_watcher, N},
        start => {migraterl_watcher, start_link, [Spec]},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [migraterl_watcher]
    }.
