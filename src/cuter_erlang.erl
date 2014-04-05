%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang).

-export([abs/1, element/2, length/1, make_tuple/2, max/2, min/2]).

%% erlang:abs/1
-spec abs(float() | integer()) -> float() | integer().
abs(X) when is_integer(X); is_float(X) ->
  case X >= 0 of
    true -> X;
    false -> -X
  end.

%% erlang:element/2
-spec element(integer(), tuple()) -> any().
element(N, T) when is_tuple(T); is_integer(N); N >= 1 ->
  L = erlang:tuple_to_list(T),
  find_nth_element(N, L).

find_nth_element(1, [H|_]) -> H;
find_nth_element(N, [_|T]) when N > 1 -> find_nth_element(N-1, T).

%% erlang:length/1
-spec length(list()) -> integer().
length(L) when is_list(L) -> length(L, 0).

length([], N) -> N;
length([_|L], N) -> length(L, N+1).

%% erlang:make_tuple/2
-spec make_tuple(integer(), any()) -> tuple().
make_tuple(N, X) when is_integer(N); N >= 0 -> create_tuple(N, X, []).

create_tuple(0, _, Acc) -> list_to_tuple(Acc);
create_tuple(N , X, Acc) when N > 0 -> create_tuple(N-1, X, [X|Acc]).

%% erlang:max/2
-spec max(any(), any()) -> any().
max(X, Y) ->
  case X >= Y of
    true -> X;
    false -> Y
  end.

%% erlang:mix/2
-spec min(any(), any()) -> any().
min(X, Y) ->
  case X =< Y of
    true -> X;
    false -> Y
  end.

