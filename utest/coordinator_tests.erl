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
      ?assertEqual({'ok', {ok, ok}}, R)
    end,
  Inst = 
    fun(L) ->
      [{timeout, 10, {atom_to_list(T), fun() -> Test({T, A}) end}} || {T, A} <- L]
    end,
  {foreach, Setup, [Inst]}.

