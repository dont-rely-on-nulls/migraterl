-module(migraterl_app).
-moduledoc "Application entry point.".
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    migraterl_sup:start_link().

stop(_State) ->
    ok.
