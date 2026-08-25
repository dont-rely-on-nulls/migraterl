-module(migraterl_test_log_h).

-export([adding_handler/1, changing_config/3, removing_handler/1, log/2]).

adding_handler(Config) ->
    {ok, Config}.

changing_config(_Operation, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

removing_handler(_Config) ->
    ok.

log(Event, #{config := #{owner := Owner}}) ->
    Owner ! {migraterl_test_log, Event},
    ok;
log(_Event, _Config) ->
    ok.
