-module(reduce_search_space).
-export([f/1]).

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
