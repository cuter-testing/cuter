-module(collection).
-export([f/1]).

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
