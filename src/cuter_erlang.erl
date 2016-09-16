%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang).

-export([ %% Bogus built-in operations
          atom_to_list_bogus/1
          %% Built-in operations
        , is_atom_nil/1, safe_atom_head/1, safe_atom_tail/1
        , lt_int/2, lt_float/2, unsupported_lt/2
        , safe_pos_div/2, safe_pos_rem/2
        , safe_plus/2, safe_minus/2, safe_times/2, safe_rdiv/2
        , safe_float/1, safe_list_to_tuple/1, safe_tuple_to_list/1
        , safe_hd/1, safe_tl/1
          %% Overriding functions
        , abs/1
        , float/1
        , atom_to_list/1, list_to_tuple/1, tuple_to_list/1, integer_to_list/1, list_to_integer/1
        , 'and'/2, 'andalso'/2, 'not'/1, 'or'/2, 'orelse'/2, 'xor'/2
        , 'div'/2, 'rem'/2, trunc/1, safe_trunc/1
        , element/2, setelement/3
        , length/1, tuple_size/1
        , make_tuple/2
        , max/2, min/2
        , basic_eq/2, '=:='/2, '=/='/2
        , '=='/2, '/='/2
        , '<'/2, '=<'/2, '>'/2, '>='/2
        , '+'/2, '-'/2, '*'/2, '/'/2
        , hd/1, tl/1
        , '++'/2, '--'/2, reverse/2, member/2, keyfind/3
        , is_binary/1, bit_size/1, byte_size/1
        , 'bsl'/2, 'bsr'/2, 'bnot'/1
        ]).

%% XXX When adding type constraints for spec, the overriding funs must be ignored
%% as they have the proper spec (so dialyzer won't complain).

%% ----------------------------------------------------------------------------
%% Other functions
%% ----------------------------------------------------------------------------

%%
%% Simulate erlang:element/2
%%
%% Return the term in the n-th position of a tuple.
%%

-spec element(pos_integer(), tuple()) -> term().
element(N, T) when is_tuple(T), is_integer(N) ->
  L = erlang:tuple_to_list(T),
  lists:nth(N, L).

%%
%% Simulate erlang:setelement/3
%%
%% Set the term in the n-th position of a tuple.
%%

-spec setelement(pos_integer(), tuple(), term()) -> tuple().
setelement(N, T, V) ->
  L = erlang:tuple_to_list(T),
  set_nth_element(N, V, L, 1, []).

set_nth_element(_N, _V, [], _Curr, Acc) ->
  erlang:list_to_tuple(lists:reverse(Acc));
set_nth_element(N, V, [_|T], N, Acc) ->
  set_nth_element(N, V, T, N+1, [V|Acc]);
set_nth_element(N, V, [H|T], Curr, Acc) ->
  set_nth_element(N, V, T, Curr+1, [H|Acc]).

%%
%% Simulate erlang:length/1
%%
%% Find the length of a list via iterating over it.
%%

-spec length(list()) -> non_neg_integer().
length(L) ->
  length(L, 0).

length([], N) -> N;
length([_|L], N) -> length(L, N+1).

%%
%% Simulate erlang:tuple_size/1
%%
%% Find the size of a tuple.
%%

-spec tuple_size(tuple()) -> non_neg_integer().
tuple_size(T) when is_tuple(T) ->
  ?MODULE:length(erlang:tuple_to_list(T)).

%%
%% Simulate erlang:make_tuple/2
%%
%% Create a tuple with N copies of a term.
%%

-spec make_tuple(non_neg_integer(), term()) -> tuple().
make_tuple(N, X) when is_integer(N) ->
  case lt_int(N, 0) of
    true  -> error(badarg);
    false -> create_tuple(N, X, [])
  end.

create_tuple(0, _, Acc) -> erlang:list_to_tuple(Acc);
create_tuple(N, X, Acc) -> create_tuple(N-1, X, [X|Acc]).

%% ----------------------------------------------------------------------------
%% TYPE CONVERSION OPERATIONS
%% ----------------------------------------------------------------------------

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

-spec safe_atom_head(nonempty_string()) -> integer().
safe_atom_head([X|_]) -> X.

-spec safe_atom_tail(nonempty_string()) -> string().
safe_atom_tail([_|Xs]) -> Xs.

-spec atom_to_list(atom()) -> string().
atom_to_list(X) when is_atom(X) ->
  XX = atom_to_list_bogus(X),
  atom_to_list(XX, []).

-spec atom_to_list(string(), string()) -> string().
atom_to_list(X, Acc) ->
  case is_atom_nil(X) of
    true ->
      lists:reverse(Acc);
    false ->
      H = safe_atom_head(X),
      T = safe_atom_tail(X),
      atom_to_list(T, [H|Acc])
  end.

%%
%% Simulate erlang:float/1
%%
%% Ensure that the argument is a float.
%%

-spec float(number()) -> float().
float(X) when is_number(X) -> safe_float(X).

-spec safe_float(number()) -> float().
safe_float(X) -> erlang:float(X).

%%
%% Simulate erlang:list_to_tuple/1
%%
%% Ensure that the argument is a list.
%%

-spec list_to_tuple(list()) -> tuple().
list_to_tuple(X) when is_list(X) -> safe_list_to_tuple(X).

-spec safe_list_to_tuple(list()) -> tuple().
safe_list_to_tuple(X) -> erlang:list_to_tuple(X).

%%
%% Simulate erlang:tuple_to_list/1
%%
%% Ensure that the argument is a tuple.
%%

-spec tuple_to_list(tuple()) -> list().
tuple_to_list(X) when is_tuple(X) -> safe_tuple_to_list(X).

-spec safe_tuple_to_list(tuple()) -> list().
safe_tuple_to_list(X) -> erlang:tuple_to_list(X).

%%
%% Simulate erlang:integer_to_list/1
%%

-spec int_to_char(0..9) -> 48..57.
int_to_char(0) -> $0;
int_to_char(1) -> $1;
int_to_char(2) -> $2;
int_to_char(3) -> $3;
int_to_char(4) -> $4;
int_to_char(5) -> $5;
int_to_char(6) -> $6;
int_to_char(7) -> $7;
int_to_char(8) -> $8;
int_to_char(9) -> $9.

-spec integer_to_list(integer()) -> [45 | 48..57,...].
integer_to_list(I) when is_integer(I) ->
  case lt_int(I, 0) of
    true  -> [$- | safe_integer_to_list(-I, [])];
    false -> safe_integer_to_list(I, [])
  end.

-spec safe_integer_to_list(non_neg_integer(), [48..57]) -> [48..57,...].
safe_integer_to_list(I, Acc) when I < 10 ->
  [int_to_char(I) | Acc];
safe_integer_to_list(I, Acc) ->
  D = int_to_char(I rem 10),
  safe_integer_to_list(I div 10, [D|Acc]).

-spec list_to_integer([43 | 45 | 48..57, ...]) -> integer().
list_to_integer([]) -> erlang:error(badarg);
list_to_integer([$-]) -> erlang:error(badarg);
list_to_integer([$+]) -> erlang:error(badarg);
list_to_integer([$-|L]) -> -1 * list_to_integer_10(L, 0);
list_to_integer([$+|L]) -> list_to_integer_10(L, 0);
list_to_integer(L) -> list_to_integer_10(L, 0).

list_to_integer_10([], Acc) -> Acc;
list_to_integer_10([H|T], Acc) when H >= $0 andalso H =< $9 ->
  list_to_integer_10(T, 10 * Acc + (H - $0));
list_to_integer_10(_, _) -> erlang:error(badarg).

%% ----------------------------------------------------------------------------
%% ARITHMETIC OPERATIONS
%% ----------------------------------------------------------------------------

%%
%% Simulate erlang:'+'/2
%%
%% Ensure that both operands are numbers.
%%

-spec '+'(number(), number()) -> number().
'+'(X, Y) when is_number(X), is_number(Y) ->
  safe_plus(X, Y).

-spec safe_plus(number(), number()) -> number().
safe_plus(X, Y) -> X + Y.

%%
%% Simulate erlang:'-'/2
%%
%% Ensure that both operands are numbers.
%%

-spec '-'(number(), number()) -> number().
'-'(X, Y) when is_number(X), is_number(Y) ->
  safe_minus(X, Y).

-spec safe_minus(number(), number()) -> number().
safe_minus(X, Y) -> X - Y.

%%
%% Simulate erlang:'*'/2
%%
%% Ensure that both operands are numbers.
%%

-spec '*'(number(), number()) -> number().
'*'(X, Y) when is_number(X), is_number(Y) ->
  safe_times(X, Y).

-spec safe_times(number(), number()) -> number().
safe_times(X, Y) -> X * Y.

%%
%% Simulate erlang:'/'/2
%%
%% Ensure that both operands are numbers and that the denominator
%% is not zero.
%%

-spec '/'(number(), number()) -> float().
'/'(X, Y) when is_number(X), is_number(Y), Y /= 0 ->
  safe_rdiv(X, Y).

-spec safe_rdiv(number(), number()) -> float().
safe_rdiv(X, Y) -> X / Y.

%%
%% Simulate erlang:abs/1
%%
%% Find the absolute value of a number.
%%

-spec abs(integer()) -> integer()
       ; (float()) -> float().
abs(X) when is_integer(X) ->
  case lt_int(X, 0) of
    false -> X;
    true  -> -X
  end;
abs(X) when is_float(X) ->
  case lt_float(X, 0.0) of
    false -> X;
    true  -> -X
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
%% In order to be consistent with Erlang, we call the safe_pos_div/2 function
%% that returns the quotient of the integer division of two natural numbers
%% and then we set the proper sign.
%%

-spec safe_pos_div(non_neg_integer(), pos_integer()) -> non_neg_integer().
safe_pos_div(X, Y) -> X div Y.

-spec 'div'(integer(), integer()) -> integer().
'div'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y > 0 ->
  safe_pos_div(X, Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y < 0 ->
  safe_pos_div(-X, -Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y < 0 ->
  - safe_pos_div(X, -Y);
'div'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y > 0 ->
  - safe_pos_div(-X, Y).

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
%% In order to be consistent with Erlang, we call the safe_pos_rem/2 function
%% that returns the remainder of the integer division of two natural numbers
%% and then we set the proper sign.
%%

-spec safe_pos_rem(non_neg_integer(), pos_integer()) -> non_neg_integer().
safe_pos_rem(X, Y) -> X rem Y.

-spec 'rem'(integer(), integer()) -> integer().
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y > 0 ->
  safe_pos_rem(X, Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y < 0 ->
  - safe_pos_rem(-X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X >= 0, Y < 0 ->
  safe_pos_rem(X, -Y);
'rem'(X, Y) when is_integer(X), is_integer(Y), X < 0, Y > 0 ->
  - safe_pos_rem(-X, Y).

%%
%% Simulate erlang:trunc/1
%%
%% Truncate a number.
%%

-spec trunc(number()) -> integer().
trunc(X) when is_number(X) -> safe_trunc(X).

-spec safe_trunc(number()) -> integer().
safe_trunc(X) -> erlang:trunc(X).

%% ----------------------------------------------------------------------------
%% BOOLEAN OPERATIONS
%% ----------------------------------------------------------------------------

%%
%% Simulate erlang:'not'/1
%%

-spec 'not'(boolean()) -> boolean().
'not'(X) ->
  case X of
    true  -> false;
    false -> true;
    _ -> error(badarg)
  end.

%%
%% Simulate erlang:'and'/2
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
%% The short-circuited 'and' logical operator.
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
%% Simulate erlang:'or'/2
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

%% ----------------------------------------------------------------------------
%% COMPARISONS
%%
%% The global term order is
%% number < atom < reference < fun < port < pid < tuple < map < list < bit string
%% ----------------------------------------------------------------------------

-spec basic_eq(term(), term()) -> boolean().
basic_eq(X, Y) -> X =:= Y.

-spec '=:='(term(), term()) -> boolean().
'=:='([], []) ->
  true;
'=:='([H1|T1], [H2|T2]) ->
  '=:='(H1, H2) andalso '=:='(T1, T2);
'=:='(L1, L2) when is_list(L1); is_list(L2) ->
  false;
'=:='(T1, T2) when is_tuple(T1), is_tuple(T2) ->
  '=:='(erlang:tuple_to_list(T1), erlang:tuple_to_list(T2));
'=:='(T1, T2) when is_tuple(T1); is_tuple(T2) ->
  false;
'=:='(X, Y) ->
  basic_eq(X, Y).

-spec '=/='(term(), term()) -> boolean().
'=/='(X, Y) -> not '=:='(X, Y).

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

-spec '=='(term(), term()) -> boolean().
'=='(X, Y) when is_integer(X), is_float(Y) ->
  erlang:float(X) =:= Y;
'=='(X, Y) when is_float(X), is_integer(Y) ->
  X =:= erlang:float(Y);
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

-spec '/='(term(), term()) -> boolean().
'/='(X, Y) when is_integer(X), is_float(Y) ->
  erlang:float(X) =/= Y;
'/='(X, Y) when is_float(X), is_integer(Y) ->
  X =/= erlang:float(Y);
'/='(X, Y) ->
  X =/= Y.

%%
%% Simulate erlang:'<'/2
%%
%% The auxiliary built-in operations defined are:
%%   * lt_int/2
%%     Compare two integers.
%%   * lt_float/2
%%     Compare two floats.
%%   * unsupported_lt/2
%%     Compare two terms, where the left operand cannot be understood by the
%%     solver.
%%

-spec lt_int(integer(), integer()) -> boolean().
lt_int(X, Y) -> X < Y.

-spec lt_float(float(), float()) -> boolean().
lt_float(X, Y) -> X < Y.

%% Used to wrap a call to erlang'<'/2 when the solver cannot 'understand' the
%% left operand. i.e. when it's a term like reference, fun, pid etc.
-type ut() :: reference() | function() | port() | pid() | map() | bitstring().
-spec unsupported_lt(ut(), term()) -> boolean().
unsupported_lt(X, Y) -> X < Y.

-spec lt_atom(string(), string()) -> boolean().
lt_atom([], []) ->
  false;
lt_atom(_, []) ->
  false;
lt_atom([], _) ->
  true;
lt_atom([X|Xs], [X|Ys]) ->
  lt_atom(Xs, Ys);
lt_atom([X|_Xs], [Y|_Ys]) ->
  lt_int(X, Y).

-spec lt_tuple([term()], [term()]) -> boolean().
lt_tuple([], []) ->
  false;
lt_tuple([X|Xs], [Y|Ys]) ->
  case X == Y of
    true  -> lt_tuple(Xs, Ys);
    false -> ?MODULE:'<'(X, Y)
  end.

-spec lt_list([term()], [term()]) -> boolean().
lt_list([], []) ->
  false;
lt_list(_, []) ->
  false;
lt_list([], _) ->
  true;
lt_list([X|Xs], [Y|Ys]) ->
  case X == Y of
    true  -> lt_list(Xs, Ys);
    false -> ?MODULE:'<'(X, Y)
  end.

-spec lt_bitstring(bitstring(), bitstring()) -> boolean().
lt_bitstring(<<>>, <<>>) ->
  false;
lt_bitstring(<<>>, _) ->
  true;
lt_bitstring(_, <<>>) ->
  false;
lt_bitstring(<<1:1, _/bitstring>>, <<0:1, _/bitstring>>) ->
  false;
lt_bitstring(<<0:1, _/bitstring>>, <<1:1, _/bitstring>>) ->
  true;
lt_bitstring(<<_:1, Xs/bitstring>>, <<_:1, Ys/bitstring>>) ->
  lt_bitstring(Xs, Ys).

-type t() :: atom() | function() | map() | maybe_improper_list()
           | number() | pid() | port() | reference()| tuple().
-spec '<'(t(), term()) -> boolean().
%% Left operand is a number.
%% Comparing integers and floats will convert the term with the lesser
%% precision into the other term's type before the comparison.
'<'(X, Y) when is_integer(X), is_integer(Y) ->
  lt_int(X, Y);
'<'(X, Y) when is_integer(X), is_float(Y) ->
  lt_float(erlang:float(X), Y);
'<'(X, Y) when is_float(X), is_integer(Y) ->
  lt_float(X, erlang:float(Y));
'<'(X, Y) when is_float(X), is_float(Y) ->
  lt_float(X, Y);
'<'(X, _Y) when is_number(X) ->
  true;
%% Left operand is an atom.
%% Atoms are lexicographically compared.
'<'(X, Y) when is_atom(X), is_number(Y) ->
  false;
'<'(X, Y) when is_atom(X), is_atom(Y) ->
  LX = ?MODULE:atom_to_list(X),
  LY = ?MODULE:atom_to_list(Y),
  lt_atom(LX, LY);
'<'(X, _Y) when is_atom(X) ->
  true;
%% Left operand is a tuple.
%% Tuples are ordered by size. When two tuples have the same size, they
%% are compared element by element.
'<'(X, Y) when is_tuple(X), is_tuple(Y) ->
  SX = ?MODULE:tuple_size(X),
  SY = ?MODULE:tuple_size(Y),
  case SX =:= SY of
    false ->
      lt_int(SX, SY);
    true ->
      LX = erlang:tuple_to_list(X),
      LY = erlang:tuple_to_list(Y),
      lt_tuple(LX, LY)
  end;
'<'(X, Y) when is_tuple(X) andalso (      is_number(Y)
                                   orelse is_atom(Y)
                                   orelse is_reference(Y)
                                   orelse is_function(Y)
                                   orelse is_port(Y)
                                   orelse is_pid(Y)
                                   ) ->
  false;
'<'(X, _Y) when is_tuple(X) ->
  true;
%% Left operand is a list.
%% Lists are compared element by element.
'<'(X, Y) when is_list(X), is_list(Y) ->
  lt_list(X, Y);
'<'(X, Y) when is_list(X) andalso (      is_number(Y)
                                  orelse is_atom(Y)
                                  orelse is_reference(Y)
                                  orelse is_function(Y)
                                  orelse is_port(Y)
                                  orelse is_pid(Y)
                                  orelse is_tuple(Y)
                                  orelse is_map(Y)
                                  ) ->
  false;
'<'(X, _Y) when is_list(X) ->
  true;
'<'(X, Y) when is_bitstring(X), is_bitstring(Y) ->
  lt_bitstring(X, Y);
'<'(X, _Y) when is_bitstring(X) ->
  false;
%% Left operand is a reference, a function, a port, a pid, or a map.
%% These terms are 'incomprehensible' for the solver, thus we directly call
%% the erlang:'<'/2 operator.
%'<'(X, Y) when is_reference(X); is_function(X); is_port(X); is_pid(X); is_map(X) ->
'<'(X, Y) ->
  unsupported_lt(X, Y).

%%
%% Simulate erlang:'=<'/2
%%

-spec '=<'(term(), term()) -> boolean().
'=<'(X, Y) ->
  case ?MODULE:'=='(X, Y) of
    true  -> true;
    false -> ?MODULE:'<'(X, Y)
  end.

%%
%% Simulate erlang:'>'/2
%%

-spec '>'(term(), term()) -> boolean().
%'>'(X, Y) ->
%  ?MODULE:'not'(?MODULE:'=<'(X, Y)).
'>'(X, Y) when is_integer(X), is_integer(Y) ->
  lt_int(Y, X);
'>'(X, Y) when is_integer(X), is_float(Y) ->
  lt_float(Y, erlang:float(X));
'>'(X, Y) when is_float(X), is_integer(Y) ->
  lt_float(erlang:float(Y), X);
'>'(X, Y) when is_float(X), is_float(Y) ->
  lt_float(Y, X);
'>'(X, _Y) when is_number(X) ->
  false;
%% Left operand is an atom.
%% Atoms are lexicographically compared.
'>'(X, Y) when is_atom(X), is_number(Y) ->
  true;
'>'(X, Y) when is_atom(X), is_atom(Y) ->
  LX = ?MODULE:atom_to_list(X),
  LY = ?MODULE:atom_to_list(Y),
  lt_atom(LY, LX);
'>'(X, _Y) when is_atom(X) ->
  false;
%% Left operand is a tuple.
%% Tuples are ordered by size. When two tuples have the same size, they
%% are compared element by element.
'>'(X, Y) when is_tuple(X), is_tuple(Y) ->
  SX = ?MODULE:tuple_size(X),
  SY = ?MODULE:tuple_size(Y),
  case SX =:= SY of
    false ->
      lt_int(SY, SX);
    true ->
      LX = erlang:tuple_to_list(X),
      LY = erlang:tuple_to_list(Y),
      lt_tuple(LY, LX)
  end;
'>'(X, Y) when is_tuple(X) andalso (      is_number(Y)
                                   orelse is_atom(Y)
                                   orelse is_reference(Y)
                                   orelse is_function(Y)
                                   orelse is_port(Y)
                                   orelse is_pid(Y)
                                   ) ->
  true;
'>'(X, _Y) when is_tuple(X) ->
  false;
%% Left operand is a list.
%% Lists are compared element by element.
'>'(X, Y) when is_list(X), is_list(Y) ->
  lt_list(Y, X);
'>'(X, Y) when is_list(X) andalso (      is_number(Y)
                                  orelse is_atom(Y)
                                  orelse is_reference(Y)
                                  orelse is_function(Y)
                                  orelse is_port(Y)
                                  orelse is_pid(Y)
                                  orelse is_tuple(Y)
                                  orelse is_map(Y)
                                  ) ->
  true;
'>'(X, _Y) when is_list(X) ->
  false;
%% Left operand is a reference, a function, a port, a pid, a map or a bistring.
%% These terms are 'incomprehensible' for the solver, thus we directly call
%% the erlang:'<'/2 operator.
%'<'(X, Y) when is_reference(X); is_function(X); is_port(X); is_pid(X); is_map(X); is_bitstring(X) ->
'>'(X, Y) ->
  unsupported_lt(Y, X).

%%
%% Simulate erlang'>='/2
%%

-spec '>='(t(), term()) -> boolean().
'>='(X, Y) ->
  ?MODULE:'not'(?MODULE:'<'(X, Y)).

%%
%% Simulate erlang:max/2
%%
%% Compare two terms and return the maximum.
%%

-spec max(term(), term()) -> term().
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

-spec min(term(), term()) -> term().
min(X, Y) ->
  case X =< Y of
    true  -> X;
    false -> Y
  end.

%% ----------------------------------------------------------------------------
%% LIST OPERATIONS
%% ----------------------------------------------------------------------------

%%
%% Simulate erlang:hd/1
%%
-spec hd([term(), ...]) -> term().
hd([H|_]) -> H;
hd(_) -> error(badarg).

-spec safe_hd([term(), ...]) -> term().
safe_hd(L) -> erlang:hd(L).

%%
%% Simulate erlang:tl/1
%%
-spec tl([term(), ...]) -> term().
tl([_|T]) -> T;
tl(_) -> error(badarg).

-spec safe_tl([term(), ...]) -> term().
safe_tl(L) -> erlang:tl(L).

%%
%% Simulate erlang:'++'/2
%%

-spec '++'(list(), list()) -> list().
'++'(L1, L2) -> lappend(lists:reverse(L1), L2).

-spec lappend(list(), list()) -> list().
lappend([], L2) -> L2;
lappend([X|L1], L2) -> lappend(L1, [X|L2]).

%%
%% Simulate erlang:'--'/2
%%

-spec '--'(list(), list()) -> list().
'--'(L1, []) -> L1;
'--'(L1, [H|T]) -> '--'(lists:delete(H, L1), T).

%%
%% Simulate lists:reverse/2
%%

-spec reverse(list(), list()) -> list().
reverse([], X) -> X;
reverse([H|T], Y) -> reverse(T, [H|Y]).

%%
%% Simulate lists:member/2
%%

-spec member(term(), list()) -> boolean().
member(X, [X|_]) -> true;
member(X, [_|Y]) -> member(X, Y);
member(_X, []) -> false.

%%
%% Simulate lists:keyfind/3
%%

-spec keyfind(term(), pos_integer(), [tuple()]) -> tuple() | false.
keyfind(_Key, _N, []) ->
  false;
keyfind(Key, N, [H|T]) when is_tuple(H) ->
  case erlang:element(N, H) of
    Key -> H;
    _   -> keyfind(Key, N, T)
  end.

%% ----------------------------------------------------------------------------
%% BINARY / BITSTRING OPERATIONS
%% ----------------------------------------------------------------------------

-spec is_binary(any()) -> boolean().
is_binary(Bin) when is_bitstring(Bin) -> is_binary(Bin, 0);
is_binary(_) -> false.

is_binary(<<>>, 0) -> true;
is_binary(<<>>, _) -> false;
is_binary(<<_:1, Bin/bitstring>>, N) ->
  case N of
    7 -> is_binary(Bin, 0);
    _ -> is_binary(Bin, N+1)
  end.

%%
%% Simulate erlang:bit_size/1
%%
%% Calculates the size of a bitstring in bits.
-spec bit_size(bitstring()) -> non_neg_integer().
bit_size(Bin) -> bit_size(Bin, 0).

bit_size(<<>>, Sz) -> Sz;
bit_size(<<_:1, Bin/bitstring>>, Sz) -> bit_size(Bin, Sz+1).

%%
%% Simulate erlang:byte_size/1
%%
%% Calculates the size of a bitstring in bytes.
%% Rounds up if the size is not divisible by 8.
-spec byte_size(bitstring()) -> non_neg_integer().
byte_size(<<>>) -> 0;
byte_size(Bin) -> byte_size(Bin, 0, 1).

byte_size(<<>>, _, Sz) ->
  Sz;
byte_size(<<_:1, Bin/bitstring>>, N, Sz) ->
  case N of
    8 -> byte_size(Bin, 1, Sz+1);
    _ -> byte_size(Bin, N+1, Sz)
  end.

%%
%% Simulate erlang:'bsr'/2
%%
%% Calculates the bitwise right shift of X by Y bits.
-spec 'bsl'(integer(), integer()) -> integer().
'bsl'(X, Y) when is_integer(X) andalso is_integer(Y) ->
  case Y < 0 of
    true  -> bsr_h(X, -Y);
    false -> bsl_h(X, Y)
  end.

bsl_h(X, 0) -> X;
bsl_h(X, N) -> bsl_h(X + X, N - 1).

%%
%% Simulate erlang:'bsl'/2
%%
%% Calculates the bitwise left shift of X by Y bits.
-spec 'bsr'(integer(), integer()) -> integer().
'bsr'(X, Y) when is_integer(X) andalso is_integer(Y) ->
  case Y < 0 of
    true  -> bsl_h(X, -Y);
    false -> bsr_h(X, Y)
  end.

bsr_h(X, 0) -> X;
bsr_h(X, N) ->
  case X < 0 of
    true ->
      X1 = - safe_pos_div(1 - X, 2),
      bsr_h(X1, N - 1);
    false ->
      X1 = safe_pos_div(X, 2),
      bsr_h(X1, N - 1)
  end.

%%
%% Simulate erlang:'bnot'/1
%%
%% Calculates the bitwise unary not of X.
-spec 'bnot'(integer()) -> integer().
'bnot'(X) -> -X-1.
