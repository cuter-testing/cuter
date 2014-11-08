%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Load the modules
-spec just_load_test_() -> any().
just_load_test_() ->
  Setup = fun(M) -> fun() -> setup(M) end end,
  Cleanup = fun cleanup/1,
  Inst = fun just_load/1,
  [{"Load Module: " ++ atom_to_list(M), {setup, Setup(M), Cleanup, Inst}} || M <- ?MODS_LIST].

just_load({Dir, M, MDb}) ->
  R = cuter_cerl:load(M, MDb, Dir),
  Ns = ets:lookup(MDb, name),
  [{"successful loading", ?_assertEqual({ok, M}, R)},
   {"retrieve module's name", ?_assertEqual([{name, M}], Ns)}].

%% Have the proper exports
-spec load_exports_test_() -> any().
load_exports_test_() ->
  Setup = fun(M) -> fun() -> setup(M) end end,
  Cleanup = fun cleanup/1,
  Inst = fun load_exports/1,
  [{"Validate exported funs: " ++ atom_to_list(M), {setup, Setup(M), Cleanup, Inst}} || M <- ?MODS_LIST].
  
load_exports({Dir, M, MDb}) ->
  _ = cuter_cerl:load(M, MDb, Dir),
  Exp = lists:sort(M:module_info(exports)),
  MExp = lists:map(fun({F,A}) -> {M, F, A} end, Exp),
  Ns = lists:sort(ets:lookup(MDb, exported)),
  [{"check exported funs list", ?_assertEqual([{exported, MExp}], Ns)}].


%%====================================================================
%% Helper functions
%%====================================================================

setup(M) ->
  Dir = cuter_tests_lib:setup_dir(),
  MDb = ets:new(M, [ordered_set, protected]),
  {Dir, M, MDb}.

cleanup({Dir, _M, MDb}) ->
  ets:delete(MDb),
  cuter_lib:clear_and_delete_dir(Dir).

