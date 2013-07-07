%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_json).

-export([command_to_json/2, prepare_port_command/2, decode_z3_result/1, is_unbound_var/1]).

-include("concolic_prefixes.hrl").

-define(Q, $\").
-define(ENC(T, V), [$\{, ?Q, $t, ?Q, $:, ?Q, T, ?Q, $,, ?Q, $v, ?Q, $:, V, $\}]).
-define(ENC_KEY_VAL(K, V), [?Q, K, ?Q, $:, V]).
-define(ENC_ALIAS(V), [$\{, ?Q, $l, ?Q, $:, ?Q, V, ?Q, $\}]).
-define(ENC_SYMB(V), [$\{, ?Q, $s, ?Q, $:, ?Q, V, ?Q, $\}]).
-define(ENC_DICT_ENTRY(K, V), [?Q, K, ?Q, $:, V]).
-define(ENC_DICT(S), [?Q, $d, ?Q, $:, $\{, S, $\}]).
-define(ENC_CMD(Cmd, As), [$\{, ?Q, $c, ?Q, $:, ?Q, Cmd, ?Q, $,, ?Q, $a, ?Q, $:, $\[, As, $\], $\}]).

-define(IS_WHITESPACE(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_SIGN(C), (C =:= $-)).
-define(IS_DECIMAL_POINT(C), (C =:= $.)).
-define(INC_OFFSET(D), D#decoder{offset = D#decoder.offset + 1}).
-define(PUSH(X, D), D#decoder{acc = [X | D#decoder.acc]}).
-define(OFFSET(D), D#decoder.offset).

-record(decoder, {
  state,
  offset = 1,
  type = null,
  acc = []
}).

%% ============================================================================
%% External exports
%% ============================================================================

%% Decode Z3 result
-spec decode_z3_result(binary()) -> orddict:orddict().

decode_z3_result(JSON) ->
  decode_solution(JSON, #decoder{state = start}, orddict:new()).

%% Encode a Command to JSON
-spec command_to_json(term(), term()) -> binary().

command_to_json(M, [S, Vs]) when M =:= "Bkt"; M =:= "Bkl" ->
  F = fun(X, Acc) ->
    EncX = json_encode(X),
    [$,, EncX | Acc]
  end,
  [$, | Ss] = lists:foldl(F, [], Vs),
  S0 = json_encode(S),
  Str = ?ENC_CMD(M, [S0, $,, $\[, lists:reverse(Ss), $\]]),
  list_to_binary(Str);
command_to_json(Cmd, Args) when is_list(Args) ->
  F = fun(X, Acc) ->
    EncX = json_encode(X),
    [$,, EncX | Acc]
  end,
  [$, | Ss] = lists:foldl(F, [], Args),
  Str = ?ENC_CMD(Cmd, lists:reverse(Ss)),
  list_to_binary(Str).

%% Encode a Port Command to JSON
-spec prepare_port_command(atom(), term()) -> binary().

prepare_port_command(load_file, {File, From, To}) ->
  T = ?ENC_KEY_VAL($t, [?Q, "load", ?Q]),
  A0 = [?Q, File, ?Q],
  A1 = integer_to_list(From),
  A2 = integer_to_list(To),
  As = ?ENC_KEY_VAL($a, [$\[, A0, $,, A1, $,, A2, $\]]),
  L = [$\{, T, $,, As, $\}],
  list_to_binary(L);
prepare_port_command(check_model, _) ->
  T = ?ENC_KEY_VAL($t, [?Q, "check", ?Q]),
  L = [$\{, T, $\}],
  list_to_binary(L);
prepare_port_command(get_model, _) ->
  T = ?ENC_KEY_VAL($t, [?Q, "model", ?Q]),
  L = [$\{, T, $\}],
  list_to_binary(L).

%% Check if a term represents the value of an unbound variable
-spec is_unbound_var(term()) -> boolean().

is_unbound_var(?UNBOUND_VAR) -> true;
is_unbound_var(_) -> false.

%% ==============================================================================
%% Decode JSON Solution

decode_solution(JSON, #decoder{state = start}, Ms) ->
  case trim_whitespace(JSON) of
    <<$\{, Rest/binary>> -> decode_solution(Rest, #decoder{state = key}, Ms);
    _ -> throw({expected_character, $\{})
  end;
decode_solution(JSON, #decoder{state = next_or_end}, Ms) ->
  case trim_whitespace(JSON) of
    <<$\}, Rest/binary>> ->
      case trim_whitespace(Rest) of
        <<>> -> Ms;
        _ -> throw(invalid_json_object)
      end;
    <<$,, Rest/binary>> ->
      decode_solution(Rest, #decoder{state = key}, Ms);
    _ ->
      throw(parse_error)
  end;
decode_solution(JSON, D=#decoder{state=key}, Ms) ->
  O = ?OFFSET(D),
  case trim_whitespace(JSON) of
    <<?Q, Key:O/binary, ?Q, $:, Rest/binary>> ->
      K = concolic_symbolic:list_to_symbolic(binary_to_list(Key)),
      {Obj, Rest1} = decode_object(Rest, #decoder{state = start}),
      Ms1 = orddict:store(K, Obj, Ms),
      decode_solution(Rest1, #decoder{state = next_or_end}, Ms1);
    <<?Q, _:O/binary, _/binary>> ->
      decode_solution(JSON, ?INC_OFFSET(D), Ms);
    _ ->
      throw(parse_error)
  end.

decode_object(JSON, #decoder{state = start}) ->
  case trim_whitespace(JSON) of
    <<$\{, Rest/binary>> ->
      {T, Rest1} = decode_object_type(Rest, #decoder{state = key}),
      {Obj, Rest2} = decode_object_value(T, trim_separator(Rest1, $,)),
      decode_object(Rest2, #decoder{state = endpoint, acc = [Obj]});
    <<?Q, "any", ?Q, Rest/binary>> ->
      {?UNBOUND_VAR, Rest};
    _ ->
      throw(parse_error)
    end;
decode_object(JSON, #decoder{state = endpoint, acc = [Obj]}) ->
  case trim_whitespace(JSON) of
    <<$\}, Rest/binary>> -> {Obj, Rest};
    _ -> throw(parse_error)
  end.

decode_object_type(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $t, ?Q, Rest/binary>> ->
      decode_object_type(trim_separator(Rest, $:), #decoder{state = value});
    _ ->
      throw(parse_error)
  end;
decode_object_type(JSON, D=#decoder{state = value}) ->
  O = ?OFFSET(D),
  case trim_whitespace(JSON) of
    <<?Q, Typ:O/binary, ?Q, Rest/binary>> ->
      {binary_to_list(Typ), Rest};
    <<?Q, _:O/binary, _/binary>> ->
      decode_object_type(JSON, ?INC_OFFSET(D));
    _ ->
      throw(parse_error)
  end.

decode_object_value("Int", JSON) -> decode_int(JSON, #decoder{state = key});
decode_object_value("Real", JSON) -> decode_real(JSON, #decoder{state = key});
decode_object_value("List", JSON) -> decode_list(JSON, #decoder{state = key});
decode_object_value("Tuple", JSON) -> decode_tuple(JSON, #decoder{state = key});
decode_object_value("Atom", JSON) -> decode_atom(JSON, #decoder{state = key}).

decode_int(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $v, ?Q, Rest/binary>> ->
      Rest1 = trim_separator(Rest, $:),
      decode_int(trim_whitespace(Rest1), #decoder{state = value}); %% Ensure we pass a trimmed JSON string
    _ ->
      throw(parse_error)
  end;
decode_int(JSON, D=#decoder{state = value}) ->
  case JSON of
    <<I, Rest/binary>> when ?IS_DIGIT(I); ?IS_SIGN(I) ->
      decode_int(Rest, ?PUSH(I, D));
    _ ->
      I = list_to_integer(lists:reverse(D#decoder.acc)),
      {I, JSON}
  end.

decode_real(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $v, ?Q, Rest/binary>> ->
      Rest1 = trim_separator(Rest, $:),
      decode_real(trim_whitespace(Rest1), #decoder{state = value}); %% Ensure we pass a trimmed JSON string
    _ ->
      throw(parse_error)
  end;
decode_real(JSON, D=#decoder{state = value}) ->
  case JSON of
    <<I, Rest/binary>> when ?IS_DIGIT(I); ?IS_SIGN(I); ?IS_DECIMAL_POINT(I) ->
      decode_real(Rest, ?PUSH(I, D));
    _ ->
      F = list_to_float(lists:reverse(D#decoder.acc)),
      {F, JSON}
  end.

decode_list(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $v, ?Q, Rest/binary>> ->
      decode_list(trim_separator(Rest, $:), #decoder{state = value_start});
    _ ->
      throw(parse_error)
  end;
decode_list(JSON, #decoder{state = value_start}) ->
  case trim_whitespace(JSON) of
    <<$\[, $\], Rest/binary>> ->
      {[], Rest};
    <<$\[, Rest/binary>> ->
      {Obj, Rest1} = decode_object(Rest, #decoder{state = start}),
      S1 = #decoder{state = value_next_or_end, acc = [Obj]},
      decode_list(Rest1, S1);
    _ ->
      throw(parse_error)
  end;
decode_list(JSON, D=#decoder{state = value_next_or_end}) ->
  case trim_whitespace(JSON) of
    <<$\], Rest/binary>> ->
      {lists:reverse(D#decoder.acc), Rest};
    <<$,, Rest/binary>> ->
      {Obj, Rest1} = decode_object(Rest, #decoder{state = start}),
      decode_list(Rest1, ?PUSH(Obj, D));
    _ ->
      throw(parse_error)
  end.

decode_tuple(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $v, ?Q, Rest/binary>> ->
      {L, Rest1} = decode_list(trim_separator(Rest, $:), #decoder{state = value_start}),
      {list_to_tuple(L), Rest1};
    _ ->
      throw(parse_error)
  end.

decode_atom(JSON, #decoder{state = key}) ->
  case trim_whitespace(JSON) of
    <<?Q, $v, ?Q, Rest/binary>> ->
      decode_atom(trim_separator(Rest, $:), #decoder{state = value_start});
    _ ->
      throw(parse_error)
  end;
decode_atom(JSON, #decoder{state = value_start}) ->
  case trim_whitespace(JSON) of
    <<$\[, Rest/binary>> ->
      decode_atom(Rest, #decoder{state = value_next_or_end});
    _ ->
      throw(parse_error)
  end;
decode_atom(JSON, D=#decoder{state = value_next_or_end}) ->
  case trim_whitespace(JSON) of
    <<I, Rest/binary>> when ?IS_DIGIT(I); I =:= $, ->
      decode_atom(Rest, ?PUSH(I, D));
    <<$\], Rest/binary>> ->
      Ts = string:tokens(lists:reverse(D#decoder.acc), ","),
      A = list_to_atom(lists:map(fun list_to_integer/1, Ts)),
      {A, Rest};
    _ ->
      throw(parse_error)
  end.

trim_whitespace(JSON) ->
  case JSON of
    <<C, Rest/binary>> when ?IS_WHITESPACE(C) -> trim_whitespace(Rest);
    _ -> JSON
  end.
  
trim_separator(JSON, S) ->
  case trim_whitespace(JSON) of
    <<S, Rest/binary>> -> Rest;
    _ -> throw({expected_separator, S})
  end.

%% ==============================================================================


%% ==============================================================================
%% Encode Terms to JSON

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
  throw(unsupported_term_fun);
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
%% XXX Do not support other types yet!
%encode_term(T, Seen, true) when is_reference(T) -> encode_term_structure(T, Seen);
%encode_term(T, Seen, false) when is_reference(T) ->
%  case is_shared(T, Seen) of
%    {true, R} -> encode_term_alias(R);
%    false -> encode_term_structure(T, Seen)
%  end;
%encode_term(P, Seen, true) when is_pid(P) -> encode_term_structure(P, Seen);
%encode_term(P, Seen, false) when is_pid(P) ->
%  case is_shared(P, Seen) of
%    {true, R} -> encode_term_alias(R);
%    false -> encode_term_structure(P, Seen)
%  end;
encode_term(Term, _Seen, _Top) ->
  throw({unsupported_term, Term}).

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
%% XXX Comment out code to satisfy dialyzer!
%encode_term_structure(T, _Seen) when is_reference(T) ->
%  I = erlang:ref_to_list(T) -- "#Ref<>",
%  ?ENC("Ref", [?Q, I, ?Q]);
%encode_term_structure(P, _Seen) when is_pid(P) ->
%  I = pid_to_list(P) -- "<>",
%  ?ENC("Pid", [?Q, I, ?Q]).

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
