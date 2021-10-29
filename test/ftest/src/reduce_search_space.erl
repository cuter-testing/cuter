-module(reduce_search_space).
-export([f/1, f6/1, f7/2, f9/1]).

%% Functions that showcase situations where we can prune
%% the search space of executions.

-spec f(integer()) -> integer().
f(X) ->
  case g(X) of
    % The call to h can never lead to an error.
    % We can skip exploring h in this context.
    42 -> h(X)
  end.

g(X) ->
  case h(X) of
    1 -> 42;
    Y when Y > 5 -> 42;
    _ -> 0
  end.

h(6) -> 1;
h(1) -> 7;
h(3) -> 5;
h(_) -> 23.


%% Tests f1-f6 compose a graph if f6 is called. Testing f6 tests the integrity
%% of the callgraph calculation and transformation

f1(X) -> % error-free
  case X of
    3 ->
      f2(2);
    2 ->
      f2(1);
    _ -> 1
  end.

f2(X) -> % error-free
  case X of
    3 ->
      f1(2);
    2 ->
      f1(1);
    _ -> 1
  end.

f3(X) -> % possibly-erroneous
  case X of
    3 ->
      f4(2);
    2 ->
      f4(1);
    1 -> 1
  end.

f4(X) -> % possibly-erroneous
  case X of
    3 ->
      f4(2);
    2 ->
      f3(1);
    1 -> 1
  end.

f5(X) -> % possibly-erroneous
  case f1(X) of
    1 ->
      f3(X);
    _ -> 1
  end.

f6(X) ->
  case f5(X) of
    1 ->
      g(X);
    _ -> 1
  end.

f7(X, Y) -> % possibly-erroneous
  case f8(X) of % this call to f8 should not be pruned
    1 -> f8(Y); % this one should
    _ -> error("error")
  end.

f8(X) -> % error-free
  case X of
    1 -> 1;
    2 -> 2;
    _ -> 1
  end.

-spec f9(integer()) -> boolean().
f9(X) -> % error free
  f9(X, []).

-spec f9(integer(), [integer()]) -> boolean().
f9(X, Found) ->
  case X of
    1 -> true;
    _ ->
      case lists:member(X, Found) of
	false ->
	  case X rem 2 of
	    0 ->
	      f9(X div 2, [X|Found]);
	    _ ->
	      f9(3 * X + 1, [X|Found])
	  end;
	true ->
	  false
      end
  end.
