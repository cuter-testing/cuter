%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_serial_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

%% ----------------------------------------------------------------------------
%% Serializing / De-Serializing first-order values.
%% ----------------------------------------------------------------------------

-type descr() :: nonempty_string().

%% We are constructing an improper list here on purpose, hence the following:
-dialyzer({no_improper_lists, encdec_test_/0}).

-spec encdec_test_() -> [{descr(), {'setup', fun(), fun()}}].
encdec_test_() ->
  Ts = [
    {"Integers", [
      {"Positive", 42},
      {"Negative", -42}
    ]},
    {"Floats", [
      {"Positive", 3.14},
      {"Negative", -42.42}
    ]},
    {"Atoms", [
      {"Simple", ok},
      {"With Special Characters", '_@#$@#4f'}
    ]},
    {"Lists", [
      {"Empty", []},
      {"Simple", [1,2,3]},
      {"With shared subterms", [1,2,[1,2],[3,1,2]] },
      {"Improper Lists", [1,2,3,4|ok]},
      {"Random alphanumeric string", binary_to_list(base64:encode(crypto:strong_rand_bytes(42)))}
    ]},
    {"Tuples", [
      {"Empty", {}},
      {"Simple", {1,2,3}},
      {"With shared subterms", {1,2,{1,2},{3,1,2},{1,2}}}
    ]},
    %% {"PIDs", [
    %%   {"Self", self()},
    %%   {"Group leader", erlang:group_leader()}
    %% ]},
    %% {"References", [
    %%   {"Make reference", erlang:make_ref()}
    %% ]},
    {"Bitstrings", [
      {"binary", <<42>>},
      {"bitstring", <<42:42>>}
    ]},
    {"Maps", [
      {"Simple", #{42 => ok}},
      {"Empty", #{}},
      {"With shared subterms", #{[[1,2],[1,2]] => [1,2,3], ok => ok}}
    ]},
    {"Symbolic Variables", [
      {"Simple", cuter_symbolic:fresh_symbolic_var()}
    ]},
    {"Mixed", [
      {"I", {[1,2],[1,2],{[1,2],[1,2]}}},
      {"II", [1,ok,{4,1},[4,4.5],4,4.5]},
      {"III", {2.23, true, [2, 3, {234, 34, false}], {ok, fail, 424242}}},
      {"IV", [<<"true">>]}
    ]}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun encode_decode/1,
  [{"Encoding / Decoding: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

encode_decode(Terms) ->
  Enc = fun cuter_serial:from_term/1,
  Dec = fun cuter_serial:to_term/1,
  [{Descr, ?_assertEqual(maybe(T), Dec(Enc(T)))} || {Descr, T} <- Terms].

%% Special case for reference().
%% maybe(Rf) when is_reference(Rf) -> erlang:ref_to_list(Rf);
maybe(T) -> T.

%% Serializing unsupported terms tests.
-spec enc_fail_test_() -> {descr(), [{descr(), _}]}.
enc_fail_test_() ->
  Enc = fun cuter_serial:from_term/1,
  Ts = [
    {"Fun", fun() -> ok end}
  ],
  {"Encoding Fail", [{Dsr, ?_assertThrow({unsupported_term, _}, Enc(T))} || {Dsr, T} <- Ts]}.

%% ----------------------------------------------------------------------------
%% Serializing / De-Serializing log entries.
%% ----------------------------------------------------------------------------

-spec encdec_cmd_test_() -> [{descr(), {'setup', fun(), fun()}}].
encdec_cmd_test_() ->
  Cs = [
    {"I", {'OP_PARAMS', [cuter_symbolic:fresh_symbolic_var(), cuter_symbolic:fresh_symbolic_var()]}},
    {"II", {'OP_MATCH_EQUAL_FALSE', [cuter_symbolic:fresh_symbolic_var()]}},	%% , erlang:make_ref()]}},
    {"III", {'OP_PARAMS', [cuter_symbolic:fresh_symbolic_var(), 42]}},
    {"IV", {'OP_CONS', [cuter_symbolic:fresh_symbolic_var(), cuter_symbolic:fresh_symbolic_var(), [<<"false">>]]}}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun encode_decode_cmd/1,
  [{"Encoding / Decoding Command: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Cs].

encode_decode_cmd({OpCode, Args}) ->
  MaybeArgs = [maybe(A) || A <- Args],
  Enc = cuter_serial:to_log_entry(OpCode, Args, false, 0),
  Dec = cuter_serial:from_log_entry(Enc),
  [?_assertEqual( {OpCode, MaybeArgs, false, 0}, Dec )].

%% ----------------------------------------------------------------------------
%% Serializing / De-Serializing funs.
%% ----------------------------------------------------------------------------

-spec dec_fun_test_() -> [{descr(), {'setup', fun(), fun()}}].
dec_fun_test_() ->
  Ts = [
    {"1-arity", fun1()},
    {"2-arity", fun2()}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun decode_fun/1,
  [{"Decoding Fun: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

decode_fun({Bin, KVs, Def}) ->
  Dec = cuter_serial:to_term(Bin),
  CF = cuter_lib:compile_lambda(Dec),
  Arity = cuter_lib:lambda_arity(Dec),
  F =
    case Arity of
      1 -> fun([X1]) -> CF(X1) end;
      2 -> fun([X1, X2]) -> CF(X1, X2) end
    end,
  ElseKey = [self() || _ <- lists:seq(1, Arity)],
  [?_assertEqual(V, F(K)) || {K, V} <- KVs] ++ [?_assertEqual(Def, F(ElseKey))].

-spec dec_fun2_test_() -> [{descr(), _}, ...].
dec_fun2_test_() ->
  L1 = cuter_lib:mk_lambda([{[1], 2}], 42, 1),
  L2 = cuter_lib:mk_lambda([{[2], L1}], 42, 1),
  Enc = cuter_serial:from_term(L2),
  Dec = cuter_serial:to_term(Enc),
  CF = cuter_lib:compile_lambda(Dec),
  Step1 = CF(2),
  Step2 = Step1(1),
  [{"Fun as return value", ?_assertEqual(2, Step2)}].

%% helper functions

fun1() ->
  KVs = [{[1], 2}, {[ok], 3.14}, {[4], <<1>>}],
  Default = 42,
  Arity = 1,
  Lambda = cuter_lib:mk_lambda(KVs, Default, Arity),
  {cuter_serial:from_term(Lambda), KVs, Default}.

fun2() ->
  KVs = [{[1, 2], 32}, {[ok, foo], 1}, {[4, <<>>], 1}],
  Default = boo,
  Arity = 2,
  Lambda = cuter_lib:mk_lambda(KVs, Default, Arity),
  {cuter_serial:from_term(Lambda), KVs, Default}.

%% ----------------------------------------------------------------------------
%% Serializing specs.
%% ----------------------------------------------------------------------------

-spec encode_spec_test_() -> [{descr(), {'setup', fun(), fun()}}].
encode_spec_test_() ->
  Ts = [
    {"Simple", [
      cuter_types:t_function([cuter_types:t_atom()], cuter_types:t_atom_lit(ok))
    ]},
    {"Multiple", [
      cuter_types:t_function([cuter_types:t_float(), cuter_types:t_atom()], cuter_types:t_any()),
      cuter_types:t_function([cuter_types:t_any(), cuter_types:t_integer()], cuter_types:t_integer_lit(42))
    ]},
    {"Lists", [
      cuter_types:t_function([cuter_types:t_list(cuter_types:t_integer())], cuter_types:t_any()),
      cuter_types:t_function([cuter_types:t_nonempty_list(cuter_types:t_list(cuter_types:t_atom()))], cuter_types:t_nil())
    ]},
    {"Bitstring", [
      cuter_types:t_function([cuter_types:t_bitstring()], cuter_types:t_bitstring(6,8))
    ]},
    {"Tuple", [
      cuter_types:t_function([cuter_types:t_tuple([cuter_types:t_any(), cuter_types:t_any()])], cuter_types:t_tuple())
    ]},
    {"Union", [
      cuter_types:t_function([cuter_types:t_union([cuter_types:t_integer(), cuter_types:t_bitstring()])], cuter_types:t_any())
    ]},
    {"Ranges", [
      cuter_types:t_function([cuter_types:t_range(cuter_types:t_integer_lit(42), cuter_types:t_pos_inf())], cuter_types:t_any()),
      cuter_types:t_function([cuter_types:t_range(cuter_types:t_neg_inf(), cuter_types:t_integer_lit(-1))], cuter_types:t_any())
    ]},
    {"Funs", [
      cuter_types:t_function([cuter_types:t_function([cuter_types:t_atom()], cuter_types:t_integer())], cuter_types:t_function())
    ]}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun encode_spec/1,
  [{"Serialize specs: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

encode_spec(Spec) ->
  _ = cuter_serial:to_log_entry('OP_SPEC', [{Spec, []}], false, 0),
  [].
