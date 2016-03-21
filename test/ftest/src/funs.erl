-module(funs).
-export([f1/1, f2/3, f3/3, f41/3, f42/3, f5/4, f6/1, f7/2, f8/2,
         f91/3, f92/2, f10/1, f11/3, f12/1, f1ws/1, f2ws/3, f3ws/3,
         f5ws/4, f1hs/1, f13a/2, f13b/2, f14/2]).

-spec f1(fun((integer()) -> integer())) -> ok.
f1(F) ->
  case F(3) of
    42 ->
      case F(10) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f1ws(fun((integer()) -> 21..42)) -> ok.
f1ws(F) ->
  case F(3) of
    42 ->
      case F(10) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f1hs(fun((integer()) -> integer())) -> ok.
f1hs(F) ->
  Id = fun (X) -> X end,
  G = Id(F),
  case G(3) of
    42 ->
      case G(10) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f2(fun((integer()) -> integer()), integer(), integer()) -> ok.
f2(F, X, Y) ->
  case F(X) of
    42 ->
      case F(Y) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f2ws(fun((integer()) -> atom()), integer(), integer()) -> ok.
f2ws(F, X, Y) ->
  case F(X) of
    42 ->
      case F(Y) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f3(fun((integer()) -> integer()), integer(), integer()) -> ok.
f3(F, X, Y) ->
  case twice(F, X) of
    42 ->
      case twice(F, Y) of
       17 -> error(bug);
       _ -> ok
      end;
    _ -> ok
  end.

twice(F, X) -> F(F(X)).

-spec f3ws(fun((...) -> bitstring()), integer(), integer()) -> ok.
f3ws(F, X, Y) ->
  case twice(F, X) of
    42 ->
      case twice(F, Y) of
       17 -> error(bug);
       _ -> ok
      end;
    _ -> ok
  end.

-spec f41(fun((integer()) -> any()), integer(), integer()) -> ok.
f41(F, X, Y) ->
  Z = F(X),
  case Z(Y) of
    42 -> error(bug);
    _ -> ok
  end.

-spec f42(fun((integer()) -> any()), integer(), integer()) -> ok.
f42(F, X, Y) ->
  case (catch F(X)) of
    {'EXIT', _} -> ok;
    Z ->
      case Z(Y) of
        42 -> error(bug);
        _ -> ok
      end
  end.

-spec f5(fun((integer(), integer(), integer()) -> integer()), integer(), integer(), integer()) -> ok.
f5(F, X, Y, Z) ->
  case F(X, Y, Z) of
    42 ->
      case F(Z, Y, X) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f5ws(fun((integer(), integer(), atom()) -> integer()), integer(), integer(), integer()) -> ok.
f5ws(F, X, Y, Z) ->
  case F(X, Y, Z) of
    42 ->
      case F(Z, Y, X) of
        17 -> error(bug);
        _ -> ok
      end;
    _ -> ok
  end.

-spec f6(any()) -> any().
f6(X) when is_function(X, 1) -> f6(X(42));
f6(X) when X =/= 42 -> X.

-spec f7(fun((integer(), integer()) -> integer()), [integer()]) -> integer().
f7(F, L) when is_function(F, 2) ->
  case lists:foldl(F, 0, L) of
    42 -> error(bug);
    R -> R
  end.

-spec f8(fun((any()) -> boolean()), [any()]) -> any().
f8(F, L) when is_function(F, 1) ->
  L1 = lists:filter(F, L),
  hd(L1).

%-spec f91(fun( (any()) -> any()        ), any(), 1) -> any()
%       ; (fun( (any(), any()) -> any() ), any(), 2) -> any().
-spec f91(function(), any(), 1|2) -> any().
f91(F, X, 1) ->
  case F(X) of
    42 -> error(bug);
    R -> R
  end;
f91(F, X, 2) ->
  case F(X, X) of
    42 -> error(bug);
    R -> R
  end.

%-spec f92(fun( (any()) -> any()        ), any()) -> any()
%       ; (fun( (any(), any()) -> any() ), any()) -> any().
-spec f92(function(), any()) -> any().
f92(F, X) when is_function(F, 1) ->
  case F(X) of
    42 -> error(bug);
    R -> R
  end;
f92(F, X) when is_function(F, 2) ->
  case F(X, X) of
    42 -> error(bug);
    R -> R
  end.

-spec f10(fun((any()) -> any())) -> ok.
f10(F) ->
  G = fun(_) -> 1 end,
  case F(G) of
    42 -> error(bug);
    _ -> ok
  end.

-spec f11(function(), function(), any()) -> any().
f11(F, G, X) ->
  case (y(F))(X) + (y(G))(X) of
    9 -> error(bug);
    _ -> X
  end.

y(F) ->
  G = fun(H) ->
      F(fun(Z) -> (H(H))(Z) end)
    end,
  G(G).

-spec f12(function()) -> ok.
f12(F) ->
  case (F(fun lists:append/1))(1) of
    42 -> error(bug);
    _ -> ok
  end.

-spec f13a(fun(({any(), any()}) -> any()), tuple()) -> any().
f13a(F, X) ->
  case F(X) of
    1 ->
      case X of
        {1, 2, 3} -> error(unreachable_bug);
        _ -> ok
      end;
    _ ->
      case X of
        {4, 2} -> error(bug);
        _ -> ok
      end
  end.

-spec f13b(fun((<<_:2, _:_*4>>) -> any()), <<_:2, _:_*2>>) -> any().
f13b(F, X) ->
  case F(X) of
    1 ->
      case X of
        %% FIXME Add the proper type constraints to exclude this branch.
        <<5:8>> -> error(unreachable_bug);
        _ -> ok
      end;
    _ ->
      case X of
        <<5:6>> -> error(bug);
        _ -> ok
      end
  end.

-spec f14(fun(([integer(), ...]) -> [integer(), ...]), [integer(), ...]) -> ok.
f14(F, L) ->
  case F(L) of
    [1, 2, 3] -> error(bug);
    _ -> ok
  end.
