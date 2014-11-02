%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Ensure it runs properly
-spec run_test() -> ok.
run_test() ->
  R = cuter:run_once('lists', 'reverse', [[1]], 10),
  ?assertEqual(ok, R).
