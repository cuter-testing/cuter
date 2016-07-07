-module(collection).
-export([f/1, g/1, g1/1, h/1, f1/1, eval_nif/1, trunc1/1, trunc2/1, l2i/1, l2in/1]).

-type t() :: [complex_spec:int()].

-spec f(t()) -> ok.
f(L) -> f(L, []).

f([], _) -> ok;
f([H|T], Acc) ->
  case H of
    42 ->
      case Acc of
        [17,17] -> error(bug);
        [ok, ok] -> error(bug);
        _ -> ok
      end;
    _ -> f(T, [H|Acc])
  end.

-spec g(integer() | [integer()]) -> iolist().
g(42) -> error(bug);
g([42]) -> error(bug);
g(I) when is_integer(I) -> <<>>;
g(L) -> g(tl(L)).

-spec g1(integer() | [integer()]) -> <<>>.
g1(42) -> error(bug);
g1([]) -> <<>>;
g1(L) -> g(tl(L)).

-spec h(any()) -> any().
h(X) ->
  case X of
    42 -> get(X);
    84 -> os:timestamp();
    17 -> error(bug);
    _ -> X
  end.

-spec f1(number()) -> ok.
f1(X) ->
  case types_and_specs:f11(X) of
    42.0 -> error(bug);
    _ -> ok
  end.

eval_nif(T) ->
  case erts_debug:flat_size(T) of
    Sz when is_integer(Sz) -> ok
  end.

-spec trunc1(number()) -> ok.
trunc1(X) ->
  case trunc(X) of
    2 ->
      case X - 2 > 0.5 of
        true -> error(bug);
        false -> ok
      end;
    _ -> ok
  end.

-spec trunc2(number()) -> ok.
trunc2(X) ->
  case trunc(X) of
    -42 ->
      case X + 43 < 0.5 of
        true -> error(bug);
        false -> ok
      end;
    _ -> ok
  end.

-spec l2i([48..57, ...]) -> ok.
l2i(L) ->
  case list_to_integer(L) of
    42 -> error(bug);
    I when is_integer(I) -> ok
  end.

-spec l2in([45 | 43 | 48..57, ...]) -> ok.
l2in(L) ->
  case re:run(L, "^(\\+|-)?[0-9]*$") of
    nomatch -> ok;
    _ ->
      case list_to_integer(L) of
        42 -> error(bug);
        -42 -> error(bug);
        I when is_integer(I) -> ok
      end
  end.
