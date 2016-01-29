-module(whitelist).
-export([f/1, g/1, k/1, just_loop/1, just_loop2/1]).

-spec f(pos_integer()) -> pos_integer().
f(X) ->
  Z = just_loop(X),
  case X of
    424242 -> error(bug);
    _ -> Z
  end.

-spec g(pos_integer()) -> pos_integer().
g(X) ->
  Z = just_loop2(X),
  ZZ = lists:seq(1, X),
  case X of
    4242 -> error(bug);
    _ -> Z + lists:sum(ZZ)
  end.

-spec k(integer()) -> integer().
k(X) ->
  Z = no_debug_info:foo(X),
  case X of
    42 -> error(bug);
    _ -> Z
  end.

-spec just_loop(pos_integer()) -> 42.
just_loop(X) when X > 0 ->
  just_loop(X - 1);
just_loop(0) ->
  42.

-spec just_loop2(pos_integer()) -> 42.
just_loop2(X) when X > 0 ->
  just_loop(X - 2);
just_loop2(0) ->
  42.
