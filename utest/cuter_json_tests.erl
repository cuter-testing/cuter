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
    {"Symbolic Variables", [
      {"Simple", cuter_symbolic:fresh_symbolic_var()}
    ]},
    {"Mixed", [
      {"I", {[1,2],[1,2],{[1,2],[1,2]}}},
      {"II", [1,ok,{4,1},[4,4.5],4,4.5]},
      {"III", {2.23, true, [2, 3, {234, 34, false}], {ok, fail, 424242}}}
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
    {"Binary", <<42>>},
    {"Map", #{ok=>42}},
    {"Fun", fun() -> ok end}
  ],
  {"JSON Encoding Fail", [{Dsr, ?_assertThrow({unsupported_term, _}, Enc(T))} || {Dsr, T} <- Ts]}.

%% Encoding / Decoding Commands
-spec encdec_cmd_test_() -> term().
encdec_cmd_test_() ->
  Cs = [
    {"I", {1, [self(), erlang:make_ref()]}},
    {"II", {1, [cuter_symbolic:fresh_symbolic_var(), 42]}}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun encode_decode_cmd/1,
  [{"JSON Encoding / Decoding Command: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Cs].

encode_decode_cmd({OpCode, Args}) ->
  MaybeArgs = [maybe(A) || A <- Args],
  Enc = cuter_json:command_to_json(OpCode, Args),
  Dec = cuter_json:json_to_command(Enc),
  [?_assertEqual( {OpCode, MaybeArgs}, Dec )].



