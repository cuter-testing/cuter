-module(collection).
-export([f/1, g/1, g1/1, h/1, f1/1, eval_nif/1, trunc1/1, trunc2/1, l2i/1, l2in/1,
         to_upper/1, k/2, k2/3, p/1, a2l/1, test_alias/1, fclist/1, fweird/1]).

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

%% If solver returns a valid list that does not match the pattern,
%% cuter will not be lead to the inner case statement.
-spec l2in([45 | 43 | 48..57, ...]) -> ok.
l2in(L) ->
  case re:run(L, "^(\\+|-)?[0-9]+$") of
    nomatch -> ok;
    _ ->
      case list_to_integer(L) of
        42 -> error(bug);
        -42 -> error(bug);
        I when is_integer(I) -> ok
      end
  end.

%% This testcase checks that given the below spec for the mfa, CutEr does not
%% explore the path of the first clause.
%% This behaviour was reported in Issue #86. It included a slightly different
%% program and the error was related with the behaviour of erlang:is_binary/1.
-spec to_upper(string()) -> string().
to_upper(S) when is_binary(S) ->
  error(bug);
to_upper(S) when is_list(S) ->
  [char_to_upper(C) || C <- S];
to_upper(C) when is_integer(C) ->
  char_to_upper(C).

char_to_upper(16#0061) -> 16#0041;
char_to_upper(16#0062) -> 16#0042;
char_to_upper(C) -> C.

-spec k(integer(), integer()) -> ok.
k(X, Y) ->
  Z = {X, Y},
  case list_to_tuple(tuple_to_list(Z)) of
    {1, 2} -> error(bug);
    _ -> ok
  end.

-spec k2(number(), pos_integer(), integer()) -> ok.
k2(X, Y, Z) ->
  case X / Y of
      0.7 -> error(bug);
      _ ->
        case Z rem Y of
          3 -> error(bug);
          _ -> ok
        end
  end.

-spec p(integer()) -> ok.
p(X) ->
  case math:pow(X, 1) of  %% The solver can only handle trivial cases.
    8.0 -> error(bug);
    _ -> ok
  end.

-spec a2l(atom()) -> ok.
a2l(A) ->
  case atom_to_list(A) of
      "foo" -> error(bug);
      _ -> ok
  end.

-spec test_alias(any()) -> ok.
test_alias(X) ->
  Y = [X | [[1,2],[1,2],[1,2]]],
  case hd(Y) of
      5 -> error(bug);
      _ -> ok
  end.

-type clist() :: nil | {integer(), clist()}.

-spec fclist(clist()) -> ok.
fclist(X) ->
  case X of
    1 -> error(unreachable_bug);
    {42, {17, nil}} -> error(bug);
    _ -> ok
  end.

-type weird() :: ok | [integer() | weird()].

-spec fweird(weird()) -> ok.
fweird(X) ->
  case X of
    1 -> error(unreachable_bug);
    [42, [ok], [17, ok, ok, [1, 2]]] -> error(bug);
    _ -> ok
  end.
