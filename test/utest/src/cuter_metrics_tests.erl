%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-spec distribution_metric_test() -> ok.
distribution_metric_test() ->
  Name = some_metric,
  ok = cuter_metrics:start(),
  ok = cuter_metrics:define_distribution_metric(Name),
  lists:foreach(fun (N) -> cuter_metrics:measure_distribution(Name, N) end,
                [42, 1, 42, 23, 1, 42]),
  R = cuter_metrics:get_distribution(Name),
  ok = cuter_metrics:stop(),
  D = [{42, 3}, {1, 2}, {23, 1}],
  ?assertEqual({ok, D}, R).

-spec distribution_metric_collected_test() -> ok.
distribution_metric_collected_test() ->
  ok = cuter_metrics:start(),
  ok = cuter_metrics:define_distribution_metric(xxx),
  ok = cuter_metrics:define_distribution_metric(yyy),
  ok = cuter_metrics:define_distribution_metric(zzz),
  R = cuter_metrics:get_distribution_metrics(),
  ok = cuter_metrics:stop(),
  ?assertEqual([xxx, yyy, zzz], R).

-spec distribution_metric_exists_test() -> ok.
distribution_metric_exists_test() ->
  Name = some_metric,
  ok = cuter_metrics:start(),
  ok = cuter_metrics:define_distribution_metric(Name),
  R = cuter_metrics:define_distribution_metric(Name),
  ok = cuter_metrics:stop(),
  ?assertEqual(eexist, R).

-spec measure_unknown_distribution_metric_test() -> ok.
measure_unknown_distribution_metric_test() ->
  ok = cuter_metrics:start(),
  R = cuter_metrics:measure_distribution(some_metric, 42),
  ok = cuter_metrics:stop(),
  ?assertEqual(enoent, R).
