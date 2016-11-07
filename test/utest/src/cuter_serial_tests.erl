%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_serial_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

%% Serializing / De-Serializing tests.
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
  [{"JSON Encoding / Decoding: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

encode_decode(Terms) ->
  Enc = fun cuter_serial:from_term/1,
  Dec = fun cuter_serial:to_term/1,
  [{Descr, ?_assertEqual(maybe(T), Dec(Enc(T)))} || {Descr, T} <- Terms].

%% Special case for reference().
maybe(Rf) when is_reference(Rf) -> erlang:ref_to_list(Rf);
maybe(T) -> T.

%% Serializing unsupported terms tests.
-spec enc_fail_test_() -> term().
enc_fail_test_() ->
  Enc = fun cuter_serial:from_term/1,
  Ts = [
    {"Fun", fun() -> ok end}
  ],
  {"JSON Encoding Fail", [{Dsr, ?_assertThrow({unsupported_term, _}, Enc(T))} || {Dsr, T} <- Ts]}.
