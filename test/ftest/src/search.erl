-module(search).
-export([f/1]).

f(X) ->
  case g(X) of
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
