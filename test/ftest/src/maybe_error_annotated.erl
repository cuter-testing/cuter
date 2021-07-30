-module(maybe_error_annotated).
-export([f/2]).

f(X, Y) ->
  case g(X) of          % maybe error
    1 -> g(Y);          % error free
    _ -> error("error") % maybe error
  end.

g(X) ->  % error free
  case X of
    1 -> 1;
    2 -> 2;
    _ -> 1
  end.
