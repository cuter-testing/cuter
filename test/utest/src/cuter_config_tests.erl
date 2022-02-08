%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_config_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-spec parameter_test() -> ok.
parameter_test() ->
  Name = some_parameter,
  ok = cuter_config:start(),
  ok = cuter_config:store(Name, 42),
  R = cuter_config:fetch(Name),
  ok = cuter_config:stop(),
  ?assertEqual({ok, 42}, R).

-spec fetch_unknown_parameter_test() -> ok.
fetch_unknown_parameter_test() ->
  ok = cuter_config:start(),
  R = cuter_config:fetch(some_metric),
  ok = cuter_config:stop(),
  ?assertEqual(not_found, R).
