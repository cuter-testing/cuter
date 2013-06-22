%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_json).

-define(Q, $\").
-define(ENC(T, V), [$\{, ?Q, $t, ?Q, $:, ?Q, T, ?Q, $,, ?Q, $v, ?Q, $:, V, $\}]).
-define(ENC_ALIAS(V), [$\{, ?Q, $l, ?Q, $:, ?Q, V, ?Q, $\}]).
-define(ENC_SYMB(V), [$\{, ?Q, $s, ?Q, $:, ?Q, V, ?Q, $\}]).
-define(ENC_DICT_ENTRY(K, V), [?Q, K, ?Q, $:, V]).
-define(ENC_DICT(S), [?Q, $d, ?Q, $:, $\{, S, $\}]).
-define(ENC_CMD(Cmd, As), [$\{, ?Q, $c, ?Q, $:, ?Q, Cmd, ?Q, $,, ?Q, $a, ?Q, $:, $\[, As, $\], $\}]).

-export([command_to_json/2]).

%% ==============================================================================
%% Encode Terms to JSON

-spec command_to_json(term(), term()) -> binary().

command_to_json(_Cmd, []) -> error(no_arguments_in_command);
command_to_json(Cmd, Args) when is_list(Args) ->
  F = fun(X, Acc) ->
    EncX = json_encode(X),
    [$,, EncX | Acc]
  end,
  [$, | Ss] = lists:foldl(F, [], Args),
  Str = ?ENC_CMD(Cmd, lists:reverse(Ss)),
  list_to_binary(Str).

%% Encode an Erlang Term to JSON (nested list)
json_encode(Term) ->
  case concolic_symbolic:is_symbolic(Term) of
    true  -> json_encode_symbolic(Term);
    false -> json_encode_concrete(Term)
  end.

json_encode_symbolic(Term) -> ?ENC_SYMB(concolic_symbolic:to_list(Term)).

json_encode_concrete(Term) ->
  {Seen, SharedTbl} = scan_term(Term, {gb_trees:empty(), gb_trees:empty()}),
  [$\{ | Str] = encode_term(Term, Seen, true),
  Dict = encode_shared(gb_trees:iterator(SharedTbl), Seen),
  [prefix_dict(Dict) | Str].

prefix_dict([]) -> $\{;
prefix_dict(Dict) -> [$\{, Dict, $,].

%% 1st Pass of a Term to locate the shared subterms
scan_term([H|T]=Term, Ds) ->
  case remember_term(Term, Ds) of
    {seen, Ds1} -> Ds1;
    Ds1 ->
      Ds2 = scan_term(H, Ds1),
      scan_term(T, Ds2)
  end;
scan_term(Term, Ds) when is_tuple(Term) ->
  case remember_term(Term, Ds) of
    {seen, Ds1}  -> Ds1;
    Ds1 -> scan_tuple(1, tuple_size(Term), Term, Ds1)
  end;
scan_term(Term, _Ds) when is_function(Term) ->
  error(unsupported_term);
scan_term(Term, Ds) ->
  case remember_term(Term, Ds) of
    {seen, Ds1} -> Ds1;
    Ds1 -> Ds1
  end.

scan_tuple(I, Sz, _, Ds) when I > Sz -> Ds;
scan_tuple(I, Sz, Term, Ds) ->
  E = element(I, Term),
  Ds1 = scan_term(E, Ds),
  scan_tuple(I+1, Sz, Term, Ds1).

remember_term(Term, {Seen, Shared}=Ds) ->
  case gb_trees:lookup(Term, Seen) of
    none ->
      {gb_trees:insert(Term, init, Seen), Shared};
    {value, init} ->
      R = erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>",
      {seen, {gb_trees:update(Term, R, Seen), gb_trees:insert(R, Term, Shared)}};
    {value, _R} ->
      {seen, Ds}
  end.

%% 2nd Pass of a Term to encode it
encode_term(I, Seen, true) when is_integer(I) -> encode_term_structure(I, Seen);
encode_term(I, Seen, false) when is_integer(I) ->
  case is_shared(I, Seen) of
    {true, R} -> encode_term_alias(R);
    false -> encode_term_structure(I, Seen)
  end;
encode_term(F, Seen, true) when is_float(F) -> encode_term_structure(F, Seen);
encode_term(F, Seen, false) when is_float(F) ->
  case is_shared(F, Seen) of
    {true, R} -> encode_term_alias(R);
    false -> encode_term_structure(F, Seen)
  end;
encode_term(A, Seen, true) when is_atom(A) -> encode_term_structure(A, Seen);
encode_term(A, Seen, false) when is_atom(A) ->
  case is_shared(A, Seen) of
    {true, R} -> encode_term_alias(R);
    false -> encode_term_structure(A, Seen)
  end;
encode_term(L, Seen, true) when is_list(L) -> encode_term_structure(L, Seen);
encode_term(L, Seen, false) when is_list(L) ->
  case is_shared(L, Seen) of
    {true, R} -> encode_term_alias(R);
    false -> encode_term_structure(L, Seen)
  end;
encode_term(T, Seen, true) when is_tuple(T) -> encode_term_structure(T, Seen);
encode_term(T, Seen, false) when is_tuple(T) ->
  case is_shared(T, Seen) of
    {true, R} -> encode_term_alias(R);
    false -> encode_term_structure(T, Seen)
  end;
encode_term(_Term, _Seen, _Top) ->
  error(unsupported_term).

encode_term_alias(R) -> ?ENC_ALIAS(R).

encode_term_structure(I, _Seen) when is_integer(I) ->
  ?ENC("Int", integer_to_list(I));
encode_term_structure(F, _Seen) when is_float(F) ->
  ?ENC("Real", float_to_list(F, [{decimals, 10}, compact]));
encode_term_structure(A, _Seen) when is_atom(A) ->
  F = fun(X, Acc) -> [$,, integer_to_list(X) | Acc] end,
  [$, | Ss] = lists:foldl(F, [], atom_to_list(A)),
  ?ENC("Atom", [$\[, lists:reverse(Ss), $\]]);
encode_term_structure([], _Seen) ->
  ?ENC("List", [$\[, $\]]);
encode_term_structure(L, Seen) when is_list(L) ->
  F = fun(X, Acc) ->
    S = encode_term(X, Seen, false),
    [$,, S | Acc]
  end,
  [$, | Ss] = lists:foldl(F, [], L),
  ?ENC("List", [$\[, lists:reverse(Ss), $\]]);
encode_term_structure(T, Seen) when is_tuple(T) ->
  F = fun(X, Acc) ->
    S = encode_term(X, Seen, false),
    [$,, S | Acc]
  end,
  [$, | Ss] = tuple_foldl(F, [], T),
  ?ENC("Tuple", [$\[, lists:reverse(Ss), $\]]).

is_shared(Term, Seen) ->
  case gb_trees:lookup(Term, Seen) of
    {value, init} -> false;
    {value, R} -> {true, R}
  end.

tuple_foldl(Fun, Acc, Tuple) ->
  tuple_foldl(1, tuple_size(Tuple), Fun, Acc, Tuple).

tuple_foldl(I, Sz, _Fun, Acc, _Tuple) when I > Sz ->
  Acc;
tuple_foldl(I, Sz, Fun, Acc, Tuple) ->
  E = element(I, Tuple),
  Acc1 = Fun(E, Acc),
  tuple_foldl(I+1, Sz, Fun, Acc1, Tuple).

%% Encode the shared subterms
encode_shared(Shared, Seen) ->
  case encode_shared(Shared, Seen, []) of
    [] -> [];
    [$, | Acc] -> ?ENC_DICT(Acc)
  end.

encode_shared(Shared, Seen, Acc) ->
  case gb_trees:next(Shared) of
    none -> Acc;
    {Key, Val, Shared1} ->
      S = encode_term(Val, Seen, true),
      encode_shared(Shared1, Seen, [$,, ?ENC_DICT_ENTRY(Key, S)|Acc])
  end.

%% ==============================================================================
