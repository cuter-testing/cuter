-module(coordinator_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error' | term()}.

%% Run most of the bencherl tests
-spec toy_test_() -> term().

toy_test_() ->
  Setup =
    fun() ->
      Version = 'short',
      Conf = [{'number_of_cores', 2}],
      Toys = [bang, genstress, big, ehb, ets_test, parallel, pcmark, serialmsg, timer_wheel, mbrot, ran],
      X = [{T, T:bench_args(Version, Conf)} || T <- Toys],
      [{T, A} || {T, L} <- X, A <- L]
    end,
  Test = 
    fun({Toy, Arg}) ->
      R = coordinator:run(Toy, run, [Arg, 'foo', 'bar']),
      ?_assertEqual({'ok', {ok, ok}}, R)
    end,
  Inst = 
    fun(L) ->
      [{timeout, 100, {atom_to_list(T), fun() -> Test({T, A}) end}} || {T, A} <- L]
    end,
  {foreach, Setup, [Inst]}.
  
%% Run tests on binaries
-spec binary_test_() -> term().

binary_test_() ->
  Setup =
    fun() ->
      [bs_bm, bs_simple_bm]
    end,
  Test = 
    fun(T) ->
      R = coordinator:run(T, main, [[]]),
      ?_assertEqual({'ok', {ok, ok}}, R)
    end,
  Inst = 
    fun(L) ->
      [{atom_to_list(T), fun() -> Test(T) end} || T <- L]
    end,
  {foreach, Setup, [Inst]}.
  
%% Calculate a fibonacci number
-spec calculate_fibonacci_test() -> 'ok'.

calculate_fibonacci_test() ->
  R = coordinator:run(demo, fib, [10]),
  ?assertMatch({ok, {55, _}}, R).

%% Test selective receive
-spec selective_receive_test() -> 'ok'.

selective_receive_test() ->
  R = coordinator:run(demo,selective_receive,[100]),
  ?assertEqual({ok, {ok, ok}}, R).

%% Find the minimum element of a list
-spec lists_minimum_element_test() -> 'ok'.

lists_minimum_element_test() ->
  R = coordinator:run(demo, min, [[5,1,3,2,7,6,4]]),
  ?assertMatch({ok, {1, _}}, R).
  
%% Basic communication between nodes
-spec basic_node_communication_test() -> 'ok'.

basic_node_communication_test() -> 
  [] = os:cmd("epmd -daemon"),
  R = coordinator:run(demo, distributed_pp, [lists:seq(1,10)]),
  ?assertMatch({ok, {10, _}}, R).
  

