%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

-define(MODS_LIST, [lists, dict, orddict, ets, os, string, filelib, beam_lib, cerl]).

%% Load modules
-spec load_modules_test_() -> term().
load_modules_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(self(), orddict:new(), 0, false),
    As = [load_mod(Cs, M) || M <- ?MODS_LIST],
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"load modules", As}]
  end,
  {"Just query modules once", {setup, Setup, Cleanup, Inst}}.

load_mod(Cs, M) ->
  Rep = cuter_codeserver:load(Cs, M),
  {atom_to_list(M), ?_assertMatch({ok, _}, Rep)}.

%% Load & Retrieve modules
-spec load_and_retrieve_test_() -> term().
load_and_retrieve_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(self(), orddict:new(), 0, false),
    R1 = cuter_codeserver:load(Cs, lists),
    R2 = cuter_codeserver:load(Cs, os),
    R3 = cuter_codeserver:load(Cs, lists),
    R4 = cuter_codeserver:load(Cs, os),
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"query modules", ?_assertMatch({{ok,X}, {ok,Y}, {ok,X}, {ok,Y}}, {R1, R2, R3, R4})}]
  end,
  {"Query modules multiple times", {setup, Setup, Cleanup, Inst}}.

%% Erroneous modules
-spec error_load_test_() -> term().
error_load_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(self(), orddict:new(), 0, false),
    R1 = cuter_codeserver:load(Cs, erlang),
    R2 = cuter_codeserver:load(Cs, foobar),
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"query invalid modules", ?_assertEqual({{error,preloaded}, {error, non_existing}}, {R1, R2})}]
  end,
  {"Query invalid modules", {setup, Setup, Cleanup, Inst}}.


%%====================================================================
%% Helper functions
%%====================================================================

setup() ->
  meck:new(cuter_iserver),
  meck:expect(cuter_iserver, code_logs, fun(_, _) -> ok end),
  {}.

cleanup(_) ->
  meck:unload(cuter_iserver).
