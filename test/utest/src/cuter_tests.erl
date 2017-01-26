%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests).

-include_lib("eunit/include/eunit.hrl").

-export([t0/1, t1/1, t2/2]).

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

%% Ensure it runs properly
-spec run_test() -> ok.
run_test() ->
  R = cuter:run('lists', 'reverse', [[1]], 0),
  ?assertEqual([], R).

-spec bugs_test_() -> any().
bugs_test_() ->
  Tests = [ {"Match to single value", {t0, [0], 1, [[42]]}}
          , {"Non-exhaustive pattern matching", {t1, [0], 17, [[42], [42.0]]}}
          , {"N-th element of a list to be an atom", {t2, [1, [1,2]], 25, fun check_t2/1}}
          ],
  [{"Shallow - " ++ Descr, {timeout, 20000, {setup, fun() -> Data end, fun find_bugs/1}}} || {Descr, Data} <- Tests].

find_bugs({Fn, Inp, Depth, Bugs}) ->
  Found = cuter:run(?MODULE, Fn, Inp, Depth),
  ToStr = fun(X) -> lists:flatten(io_lib:format("~w", [X])) end,
  case is_list(Bugs) of
    true ->
      [{ToStr(B), ?_assertEqual(true, lists:member(B, Found))} || B <- Bugs];
    false ->
      [?_assertEqual(true, Bugs(Found))]
  end.

%% ------------------------------------------------------------------
%% Functions with bugs for testing
%% ------------------------------------------------------------------

-spec t0(integer()) -> ok.
t0(42) -> error(bug);
t0(X) -> X.

-spec t1(number()) -> ok.
t1(X) when X > 42 -> ok;
t1(X) when X < 42 -> ok.

-spec t2(pos_integer(), list()) -> ok.
t2(N, L) when N =< length(L) ->
  case lists:nth(N, L) of
    X when is_atom(X) -> error(bug);
    _ -> ok
  end;
t2(_, _) -> ok.

check_t2(Found) ->
  lists:any(fun([X, Y]) -> is_atom(lists:nth(X, Y)) end, Found).
