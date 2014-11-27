%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang).

-export([ %% Bogus built-in operations
          atom_to_list_bogus/1
          %% Built-in operations
        , is_atom_nil/1, atom_head/1, atom_tail/1
        , gteq/2
        , pos_div/2, pos_rem/2
          %% Overriding functions
        , abs/1
        , atom_to_list/1
        , 'and'/2, 'andalso'/2, 'not'/1, 'or'/2, 'orelse'/2, 'xor'/2
        , 'div'/2, 'rem'/2
        , element/2
        , length/1
        , make_tuple/2
        , max/2, min/2
        , '=='/2, '/='/2
        ]).

%% ----------------------------------------------------------------------------
%% BIFs
%%
%% NOTE: There is no need to validate the arguments of each function as it
%% is only called internally .
%% ----------------------------------------------------------------------------

-spec gteq(number(), number()) -> boolean().
gteq(X, Y) -> X >= Y.

%% ----------------------------------------------------------------------------
%% Other functions
%% ----------------------------------------------------------------------------

%%
%% Simulate erlang:abs/1
%%
%% Find the absolute value of a number.
%%

-spec abs(integer()) -> integer()
       ; (float()) -> float().
abs(X) when is_integer(X); is_float(X) ->
  case gteq(X, 0) of
    true  -> X;
    false -> -X
  end.

%%
%% Simulate erlang:'and'/2
%%
%% We simulate the 'and' logical operator.
%%

-spec 'and'(boolean(), boolean()) -> boolean().
'and'(X, Y) ->
  case X of
    true ->
      case Y of
        true  -> true;
        false -> false;
        _ -> error(badarg)
      end;
    false ->
      case Y of
        true  -> false;
        false -> false;
        _ -> error(badarg)
      end;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'andalso'/2
%%
%% We simulate the short-circuited 'and' logical operator.
%%

-spec 'andalso'(boolean(), boolean()) -> boolean().
'andalso'(X, Y) ->
  case X of
    false -> false;
    true ->
      case Y of
        true  -> true;
        false -> false;
        _ -> error(badarg)
      end;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'div'/2
%%
%% The integer division in Erlang for negative integers can produce negative
%% quotients and remainders.
%%
%% e.g.
%%  12345 =  293 *  42 + 39
%% -12345 = -293 *  42 - 39
%%  12345 = -293 * -42 + 39
%% -12345 =  293 * -42 - 39
%%
%% In order to be consistent with Erlang, we call the pos_div/2 function that
%% returns the quotient of the integer division of two natural numbers and
%% then we set the proper sign.
%%

-spec pos_div(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
pos_div(X, Y) -> X div Y.

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
%% Simulate erlang:element/2
%%
%% Return the term in the n-th position of a tuple.
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
%% Simulate erlang:length/1
%%
%% Find the length of a list via iterating over it.
%%

-spec length(list()) -> integer().
length(L) when is_list(L) ->
  length(L, 0).

length([], N) -> N;
length([_|L], N) -> length(L, N+1).

%%
%% Simulate erlang:make_tuple/2
%%
%% Create a tuple with N copies of a term.
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
%% Simulate erlang:max/2
%%
%% Compare two terms and return the maximum.
%%

-spec max(any(), any()) -> any().
max(X, Y) ->
  case X >= Y of
    true  -> X;
    false -> Y
  end.

%%
%% Simulate erlang:min/2
%%
%% Compare two terms and return the minimum.
%%

-spec min(any(), any()) -> any().
min(X, Y) ->
  case X =< Y of
    true  -> X;
    false -> Y
  end.

%%
%% Simulate erlang:'not'/1
%%
%% We simulate the 'not' logical operator.
%%

-spec 'not'(boolean()) -> boolean().
'not'(X) ->
  case X of
    true  -> false;
    false -> true;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'or'/2
%%
%% We simulate the 'or' logical operator.
%%

-spec 'or'(boolean(), boolean()) -> boolean().
'or'(X, Y) ->
  case X of
    true ->
      case Y of
        true  -> true;
        false -> true;
        _ -> error(badarg)
      end;
    false ->
      case Y of
        true  -> true;
        false -> false;
        _ -> error(badarg)
      end;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'orelse'/2
%%
%% We simulate the short-circuited 'or' logical operator.
%%

-spec 'orelse'(boolean(), boolean()) -> boolean().
'orelse'(X, Y) ->
  case X of
    true -> true;
    false ->
      case Y of
        true  -> true;
        false -> false;
        _ -> error(badarg)
      end;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'rem'/2
%%
%% The integer division in Erlang for negative integers can produce negative
%% quotients and remainders.
%%
%% e.g.
%%  12345 =  293 *  42 + 39
%% -12345 = -293 *  42 - 39
%%  12345 = -293 * -42 + 39
%% -12345 =  293 * -42 - 39
%%
%% In order to be consistent with Erlang, we call the pos_rem/2 function that
%% returns the remainder of the integer division of two natural numbers and
%% then we set the proper sign.
%%

-spec pos_rem(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
pos_rem(X, Y) -> X rem Y.

-spec 'rem'(integer(), integer()) -> integer().
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y >= 0 ->
  pos_rem(X, Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y < 0 ->
  - pos_rem(-X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y < 0 ->
  pos_rem(X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y >= 0 ->
  - pos_rem(-X, Y).

%%
%% Simulate erlang:'xor'/2
%%
%% The truth table for erlang:'xor'/2 is the following:
%%
%%  X Y | X xor Y
%% -----+---------
%%  T T |    F
%%  T F |    T
%%  F T |    T
%%  F F |    F
%%

-spec 'xor'(boolean(), boolean()) -> boolean().
'xor'(X, Y) ->
  case X of
    true ->
      case Y of
        true  -> false;
        false -> true;
        _ -> error(badarg)
      end;
    false ->
      case Y of
        true  -> true;
        false -> false;
        _ -> error(badarg)
      end;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'=='/2
%%
%% The difference of erlang:'=='/2 with erlang:'=:='/2 is when comparing
%% an integer to a float.
%%
%% e.g.
%% 4 =:= 4.0 => false
%% 4 == 4.0  => true
%%
%% Therefore, in such cases, we convert the integer to float and then call
%% erlang:'=:='/2 for the comparison.
%%

-spec '=='(any(), any()) -> boolean().
'=='(X, Y) when is_integer(X), is_float(Y) ->
  float(X) =:= Y;
'=='(X, Y) when is_float(X), is_integer(Y) ->
  X =:= float(Y);
'=='(X, Y) ->
  X =:= Y.

%%
%% Simulate erlang:'/='/2
%%
%% The difference of erlang:'/='/2 with erlang:'=/='/2 is when comparing
%% an integer to a float.
%%
%% e.g.
%% 4 =/= 4.0 => true
%% 4 /= 4.0  => false
%%
%% Therefore, in such cases, we convert the integer to float and then call
%% erlang:'=/='/2 for the comparison.
%%

-spec '/='(any(), any()) -> boolean().
'/='(X, Y) when is_integer(X), is_float(Y) ->
  float(X) =/= Y;
'/='(X, Y) when is_float(X), is_integer(Y) ->
  X =/= float(Y);
'/='(X, Y) ->
  X =/= Y.

%%
%% Simulate erlang:atom_to_list/1
%%
%% The conversion from atom to list cannot be simply translated to Z3 axioms.
%% In addition, it is highly recommended to avoid dynamically creating new atoms.
%% Therefore, we do the following:
%% 
%% * Convert the atom to string in Erlang with atom_to_list_bogus/1.
%%   However, this will be translated to an identity function for the solver.
%% * Iterate over the string in Erlang and accumulate each character using
%%   is_atom_nil/1, atom_head/1, atom_tail/1.
%%   These 3 functions will be directly translated in the solver as functions
%%   that operate on atoms.

-spec atom_to_list_bogus(atom()) -> string().
atom_to_list_bogus(X) ->
  erlang:atom_to_list(X).

-spec is_atom_nil(string()) -> boolean().
is_atom_nil([]) -> true;
is_atom_nil(_)  -> false.

-spec atom_head(string()) -> integer().
atom_head([X|_]) -> X.

-spec atom_tail(string()) -> string().
atom_tail([_|Xs]) -> Xs.

-spec atom_to_list(atom()) -> string().
atom_to_list(X) ->
  XX = atom_to_list_bogus(X),
  atom_to_list(XX, []).

-spec atom_to_list(string(), string()) -> string().
atom_to_list(X, Acc) ->
  case is_atom_nil(X) of
    true ->
      lists:reverse(Acc);
    false ->
      H = atom_head(X),
      T = atom_tail(X),
      atom_to_list(T, [H|Acc])
  end.

