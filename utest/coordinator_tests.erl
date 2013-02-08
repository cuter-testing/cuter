-module(coordinator_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error' | term()}.

-spec toy_test_() -> term().
toy_test_() ->
  Setup =
    fun() ->
      Version = 'short',
      Conf = [{'number_of_cores', 2}],
      Toys = [bang, genstress, big, ehb, ets_test, parallel, pcmark, serialmsg, timer_wheel],
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
      [{timeout, 10, {atom_to_list(T), fun() -> Test({T, A}) end}} || {T, A} <- L]
    end,
  {foreach, Setup, [Inst]}.
  
-spec simple_test_() -> term().
simple_test_() ->
  Fib2 = concolic_symbolic:mock_bif({'erlang', '+', 2}, [1, 0], 1),
  Fib3 = concolic_symbolic:mock_bif({'erlang', '+', 2}, [Fib2, 1], 2),
  SR_1 = concolic_symbolic:mock_bif({'erlang', '+', 2}, [Fib3, Fib2], 3),
  R_1 = coordinator:run(demo, fib, [4]),
  {"Fibonacci", fun() -> ?_assertEqual({'ok', {3, SR_1}}, R_1) end}.
  
-spec lists_test_() -> term().
lists_test_() ->
  Args_1 = [[5,1,3,2,7,6,4]],
  [SVar_1] = concolic_symbolic:abstract(Args_1),
  X_1 = concolic_symbolic:mock_bif({'erlang', 'tl', 1}, [SVar_1], [1,3,2,7,6,4]),
  SR_1 = concolic_symbolic:mock_bif({'erlang', 'hd', 1}, [X_1], 1),
  R_1 = coordinator:run(demo, min, Args_1),
  {"Minumum element", fun() -> ?_assertEqual({'ok', {1, SR_1}}, R_1) end}.
  
-spec distributed_test_() -> term().
distributed_test_() -> 
  L_1 = lists:seq(1,10),
  [SVar_1] = concolic_symbolic:abstract([L_1]),
  SR_1 = concolic_symbolic:mock_bif({'erlang', 'length', 1}, [SVar_1], 10),
  R_1 = coordinator:run(demo, distributed_pp, [L_1]),
  {"Basic Communication across nodes", fun() -> ?_assertEqual({'ok', {10, SR_1}}, R_1) end}.
  

