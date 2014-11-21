%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang).

-export([
  %% BIFs
    gteq/2
  , pos_div/2
  , pos_rem/2
  %% Other functions
  , abs/1
  , 'div'/2
  , element/2
  , length/1
  , make_tuple/2
  , max/2
  , min/2
  , 'rem'/2
]).

%% ----------------------------------------------------------------------------
%% BIFs
%%
%% NOTE: There is no need to validate the arguments of each function as it
%% is only called internally .
%% ----------------------------------------------------------------------------

-spec gteq(number(), number()) -> boolean().
gteq(X, Y) -> X >= Y.

%% Integer division with natural numbers.
-spec pos_div(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
pos_div(X, Y) -> X div Y.

%% Remainder of integer division with natural numbers.
-spec pos_rem(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
pos_rem(X, Y) -> X rem Y.

%% ----------------------------------------------------------------------------
%% Other functions
%% ----------------------------------------------------------------------------

%%
%% erlang:abs/1
%%

-spec abs(float() | integer()) -> float() | integer().
abs(X) when is_integer(X); is_float(X) ->
  case gteq(X, 0) of
    true  -> X;
    false -> -X
  end.

%%
%% erlang:'div'/2
%%

-spec 'div'(integer(), integer()) -> integer().
'div'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y >= 0 ->
  pos_div(X, Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y < 0 ->
  pos_div(-X, -Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y < 0 ->
  - pos_div(X, -Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y >= 0 ->
  - pos_div(-X, Y).

%%
%% erlang:element/2
%%

-spec element(integer(), tuple()) -> any().
element(N, T) when is_tuple(T), is_integer(N) ->
  case gteq(N, 1) of
    false ->
      error(badarg);
    true ->
      L = erlang:tuple_to_list(T),
      find_nth_element(N, L)
  end.

find_nth_element(1, [H|_]) -> H;
find_nth_element(N, [_|T]) -> find_nth_element(N-1, T).

%%
%% erlang:length/1
%%

-spec length(list()) -> integer().
length(L) when is_list(L) ->
  length(L, 0).

length([], N) -> N;
length([_|L], N) -> length(L, N+1).

%%
%% erlang:make_tuple/2
%%

-spec make_tuple(integer(), any()) -> tuple().
make_tuple(N, X) when is_integer(N) ->
  case gteq(N, 0) of
    false -> error(badarg);
    true  -> create_tuple(N, X, [])
  end.

create_tuple(0, _, Acc) -> list_to_tuple(Acc);
create_tuple(N, X, Acc) -> create_tuple(N-1, X, [X|Acc]).

%%
%% erlang:max/2
%%

-spec max(any(), any()) -> any().
max(X, Y) ->
  case X >= Y of
    true -> X;
    false -> Y
  end.

%%
%% erlang:min/2
%%

-spec min(any(), any()) -> any().
min(X, Y) ->
  case X =< Y of
    true -> X;
    false -> Y
  end.

%%
%% erlang:'rem'/2
%%

-spec 'rem'(integer(), integer()) -> integer().
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y >= 0 ->
  pos_rem(X, Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y < 0 ->
  - pos_rem(-X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y < 0 ->
  pos_rem(X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y >= 0 ->
  - pos_rem(-X, Y).
