%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_pp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

%% Encoding / Decoding tests
-spec encdec_test_() -> term().
encdec_test_() ->
  Ts = [
    {"Simple funs", [
      {"Fun I", cuter_json:mk_lambda([{[0], 42}], ok, 1), "fun(0) -> 42; (_) -> ok end"},
      {"Fun II", cuter_json:mk_lambda([{[0], 42}, {[foo], bar}], ok, 1),
        "fun(0) -> 42; (foo) -> bar; (_) -> ok end"},
      {"Fun III", cuter_json:mk_lambda([{[0, 1], 42}], ok, 2), "fun(0,1) -> 42; (_,_) -> ok end"}
    ]},
    {"Higher order", [
      {"Fun I", cuter_json:mk_lambda([{[0], cuter_json:mk_lambda([{[0], 42}], ok, 1)}], ok, 1),
        "fun(0) -> fun(0) -> 42; (_) -> ok end; (_) -> ok end"}
    ]}
  ],
  Setup = fun(T) -> fun() -> T end end,
  Inst = fun pp_lambda/1,
  [{"Pretty print labmdas: " ++ C, {setup, Setup(T), Inst}} || {C, T} <- Ts].

pp_lambda(Terms) ->
  [{Descr, ?_assertEqual(Str, cuter_pp:pp_lambda(T))} || {Descr, T, Str} <- Terms].
