%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_json_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Encoding / Decoding tests
-spec encdec_test_() -> term().
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
    {"PIDs", [
      {"Self", self()},
      {"Group leader", erlang:group_leader()}
    ]},
    {"References", [
      {"Make reference", erlang:make_ref()}
    ]},
    {"Bitstrings", [
      {"binary", <<42>>},
      {"bitstring", <<42:42>>}
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
  [{"JSON Encoding / Decoding: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

encode_decode(Terms) ->
  Enc = fun cuter_json:term_to_json/1,
  Dec = fun cuter_json:json_to_term/1,
  [{Descr, ?_assertEqual(maybe(T), Dec(Enc(T)))} || {Descr, T} <- Terms].

%% Special case for reference()
maybe(Rf) when is_reference(Rf) -> erlang:ref_to_list(Rf);
maybe(T) -> T.

%% Encoding unsupported terms tests
-spec enc_fail_test_() -> term().
enc_fail_test_() ->
  Enc = fun cuter_json:term_to_json/1,
  Ts = [
    {"Map", #{ok=>42}},
    {"Fun", fun() -> ok end}
  ],
  {"JSON Encoding Fail", [{Dsr, ?_assertThrow({unsupported_term, _}, Enc(T))} || {Dsr, T} <- Ts]}.

%% Decoding funs.
-spec dec_fun_test_() -> term().
dec_fun_test_() ->
  Ts = [
    {"1-arity", fun1()},
    {"2-arity", fun2()}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun decode_fun/1,
  [{"JSON Decoding Fun: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

decode_fun({Bin, KVs, Def}) ->
  Dec = cuter_json:json_to_term(Bin),
  Arity = cuter_json:lambda_arity(Dec),
  CF = cuter_json:compile_lambda(Dec),
  F =
    case Arity of
      1 -> fun([X1]) -> CF(X1) end;
      2 -> fun([X1, X2]) -> CF(X1, X2) end
    end,
  ElseKey = [self() || _ <- lists:seq(1, Arity)],
  [?_assertEqual(V, F(K)) || {K, V} <- KVs] ++ [?_assertEqual(Def, F(ElseKey))].

-spec dec_fun2_test_() -> term().
dec_fun2_test_() ->
  Begin = "{\"t\":9,\"v\":[",
  End = "],\"x\":1}",
  KVs1 = "{\"t\":5,\"v\":[{\"t\":4,\"v\":[{\"t\":1,\"v\":1}]},{\"t\":1,\"v\":2}]}",
  Def1 = "{\"t\":5,\"v\":[{\"t\":1,\"v\":42}]}",
  Fn1 = [Begin, KVs1, ",", Def1, End],
  KVs2 = ["{\"t\":5,\"v\":[{\"t\":4,\"v\":[{\"t\":1,\"v\":2}]},", Fn1, "]}"],
  Def2 = "{\"t\":5,\"v\":[{\"t\":1,\"v\":17}]}",
  Fn2 = [Begin, KVs2, ",", Def2, End],
  Json = list_to_binary(Fn2),
  Dec = cuter_json:json_to_term(Json),
  CF = cuter_json:compile_lambda(Dec),
  Step1 = CF(2),
  Step2 = Step1(1),
  [{"Fun as return value", ?_assertEqual(2, Step2)}].

-spec dec_fun_without_default_test_() -> term().
dec_fun_without_default_test_() ->
  Begin = "{\"t\":9,\"v\":[",
  End = "],\"x\":1}",
  KVs = "{\"t\": 5, \"v\": [{\"t\": 4, \"v\": [{\"t\": 1, \"v\": 3}]}, {\"t\": 1, \"v\": 42}]},"
    ++ "{\"t\": 5, \"v\": [{\"t\": 4, \"v\": [{\"t\": 1, \"v\": 10}]}, {\"t\": 1, \"v\": 17}]}",
  Fn = [Begin, KVs, End],
  Json = list_to_binary(Fn),
  Dec = cuter_json:json_to_term(Json),
  CF = cuter_json:compile_lambda(Dec),
  [{"Decode fun without default", [?_assertEqual(42, CF(3)), ?_assertEqual(17, CF(10)), ?_assertEqual(42, CF(4))]}].

-spec dec_fun_const_test_() -> term().
dec_fun_const_test_() ->
  Begin = "{\"t\":9,\"v\":[",
  End = "],\"x\":1}",
  KVs = "{\"t\": 5, \"v\": [{\"t\": 4, \"v\": [{\"t\": 1, \"v\": 3}]}, {\"t\": 1, \"v\": 42}]}",
  Fn = [Begin, KVs, End],
  Json = list_to_binary(Fn),
  Dec = cuter_json:json_to_term(Json),
  CF = cuter_json:compile_lambda(Dec),
  [{"Decode fun with one input", [?_assertEqual(42, CF(3)), ?_assertEqual(42, CF(10))]}].

%% Encoding / Decoding Commands
-spec encdec_cmd_test_() -> term().
encdec_cmd_test_() ->
  Cs = [
    {"I", {1, [self(), erlang:make_ref()]}},
    {"II", {1, [cuter_symbolic:fresh_symbolic_var(), 42]}},
    {"III", {56, [cuter_symbolic:fresh_symbolic_var(), cuter_symbolic:fresh_symbolic_var(), [<<"false">>]]}}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun encode_decode_cmd/1,
  [{"JSON Encoding / Decoding Command: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Cs].

encode_decode_cmd({OpCode, Args}) ->
  MaybeArgs = [maybe(A) || A <- Args],
  Enc = cuter_json:command_to_json(OpCode, Args),
  Dec = cuter_json:json_to_command(Enc),
  [?_assertEqual( {OpCode, MaybeArgs}, Dec )].

%% ----------------------------------------------------------------------------
%% Creating testcases for lambdas.
%% ----------------------------------------------------------------------------

fun1() ->
  KVs = [{[1], 2}, {[ok], 3.14}, {[4], <<1>>}],
  Default = 42,
  Arity = 1,
  gen_fun_testcase(KVs, Default, Arity).

fun2() ->
  KVs = [{[1, 2], 32}, {[ok, foo], 1}, {[4, <<>>], 1}],
  Default = boo,
  Arity = 2,
  gen_fun_testcase(KVs, Default, Arity).

gen_fun_testcase(KVs, Default, Arity) ->
  Lambda = cuter_json:mk_lambda(KVs, Default, Arity),
  {lambda_to_json_1_level(Lambda), KVs, Default}.

lambda_to_json_1_level(T) ->
  lambda_to_json_1_level(cuter_json:lambda_kvs(T),
    cuter_json:lambda_default(T), cuter_json:lambda_arity(T)).

lambda_to_json_1_level(KVs, Default, Arity) ->
  L = [{Default} | lists:reverse(KVs)],
  L1 = add_seps([cuter_json:term_to_json(T) || T <- L], []),
  Bin = concat_segments(L1, fun_end(Arity)),
  Start = fun_start(),
  <<Start/binary, Bin/binary>>.

fun_sep() ->
  <<",">>.

fun_start() ->
  <<"{\"t\":9,\"v\":[">>.

fun_end(Arity) ->
  list_to_binary(io_lib:format("],\"x\":~w}", [Arity])).

add_seps([E], Acc) ->
  lists:reverse([E | Acc]);
add_seps([E1, E2 | Rest], Acc) ->
  add_seps([E2 | Rest], [fun_sep(), E1 | Acc]).

concat_segments([], Acc) ->
  Acc;
concat_segments([Seg|Segs], Acc) ->
  concat_segments(Segs, <<Seg/binary, Acc/binary>>).
