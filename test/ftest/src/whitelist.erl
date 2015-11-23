-module(whitelist).
-export([f/1, just_loop/1]).

-spec f(pos_integer()) -> pos_integer().
f(X) ->
  Z = just_loop(X),
  case X of
    424242 -> error(bug);
    _ -> Z
  end.

-spec just_loop(pos_integer()) -> ok.
just_loop(X) when X > 0 ->
  just_loop(X - 1);
just_loop(0) ->
  ok.
