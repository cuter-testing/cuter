-module(sum).
-export([classify/1, classify2/1]).

-include_lib("proper/include/proper.hrl").

-spec prop_classify_ok() -> any().
prop_classify_ok() ->
  ?FORALL(L, list(number()), lists:member(classify(L), [negative, small, big])).

-spec classify([number()]) -> 'negative' | 'small' | 'big'.
classify(L) ->
  case lists:sum(L) of
    S when S < 0 -> negative;
    S when S < 4711 -> small;
    S when S > 4711 -> big
  end.

-spec classify2([number()]) -> 'negative' | 'small' | 'big'.
classify2(L) ->
  case lists:foldl(fun erlang:'+'/2, 0, L) of
    S when S < 0 -> negative;
    S when S < 4711 -> small;
    S when S > 4711 -> big
  end.
