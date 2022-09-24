-module(callgraph_examples).
-export([f1/1, f2/1, f3/1, f4/1]).

-spec f1(integer()) -> integer().
f1(X) ->
  f11(X).

f11(_X) -> ok.

%% =====================================

-spec f2(integer()) -> integer().
f2(X) ->
  case X of
    1 -> 1;
    _ -> f21(X - 1)
  end.

f21(X) ->
  case X of
    2 -> 2;
    _ -> f2(X)
  end.

%% =====================================

-spec f3([any()]) -> any().
f3(X) ->
  [f31(Y) || Y <- X].

f31(X) ->
  Y = f32(X-1),
  Y + 1.

f32(X) ->
  case X of
    1 -> 1 + f33(4);
    _ -> f31(X - 1)
  end.

f33(X) ->
  X + 1.

%% =====================================

-spec f4(any()) -> any().
f4(X) ->
  F = fun(Y) -> f41(Y) end,
  F(X).

f41(X) ->
  f42(X + 1).

f42(X) ->
  f4(X + 1) + f43(X).

f43(X) ->
  f41(X + 1) + f44(X).

f44(X) -> X.
