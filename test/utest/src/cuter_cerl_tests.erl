%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").
-include("include/cuter_macros.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-type descr() :: nonempty_string().
-type f_one() :: fun((term()) -> term()).
-type setup() :: {'setup', fun(() -> term()), f_one(), f_one()}.
-type ret_t() :: {descr(), setup()}.

%% Load the modules
-spec just_load_test_() -> [ret_t()].
just_load_test_() ->
  Setup = fun(M) -> fun() -> setup(M) end end,
  Cleanup = fun cleanup/1,
  Inst = fun just_load/1,
  [{"Load Module: " ++ atom_to_list(M), {setup, Setup(M), Cleanup, Inst}}
   || M <- ?MODS_LIST].

just_load({M, MDb}) ->
  TagGen = fun() -> {?BRANCH_TAG_PREFIX, 42} end,
  R = cuter_cerl:load(M, MDb, TagGen, false),
  Ns = ets:lookup(MDb, name),
  [{"successful loading", ?_assertEqual({ok, M}, R)},
   {"retrieve module's name", ?_assertEqual([{name, M}], Ns)}].

%% Have the proper exports
-spec load_exports_test_() -> [ret_t()].
load_exports_test_() ->
  Setup = fun(M) -> fun() -> setup(M) end end,
  Cleanup = fun cleanup/1,
  Inst = fun load_exports/1,
  [{"Validate exported funs: " ++ atom_to_list(M),
    {setup, Setup(M), Cleanup, Inst}} || M <- ?MODS_LIST].
  
load_exports({M, MDb}) ->
  TagGen = fun() -> {?BRANCH_TAG_PREFIX, 42} end,
  _ = cuter_cerl:load(M, MDb, TagGen, false),
  Exp = lists:sort(M:module_info(exports)),
  MExp = lists:map(fun({F,A}) -> {M, F, A} end, Exp),
  Ns = lists:sort(ets:lookup(MDb, exported)),
  [{"check exported funs list", ?_assertEqual([{exported, MExp}], Ns)}].

%% ------------------------------------------------------------------
%% Tags related tests.
%% ------------------------------------------------------------------

-spec tag_test_() -> [ret_t()].
tag_test_() ->
  Setup = fun(M) -> fun() -> setup_for_tags(M) end end,
  Cleanup = fun cleanup_for_tags/1,
  Inst = fun generate_and_collect_tags/1,
  [{"Generate and collect tags: " ++ atom_to_list(M), {setup, Setup(M), Cleanup, Inst}} || M <- ?MODS_LIST].

generate_and_collect_tags({M, Cache}) ->
  TagGen = fun cuter_codeserver:generate_tag/0,
  _ = cuter_cerl:load(M, Cache, TagGen, false),
  FoldFn = fun(Elem, Acc) ->
      case Elem of
        {{_,_,_}, Kfun} ->
          Code = cuter_cerl:kfun_code(Kfun),
          Tags = cuter_cerl:collect_feasible_tags(Code, all),
          gb_sets:union(Acc, Tags);
        _ -> Acc
      end
    end,
  FeasibleTags = ets:foldl(FoldFn, gb_sets:new(), Cache),
  N = cuter_codeserver:get_branch_counter(),
  AddedTags = gb_sets:from_list(lists:seq(1, N)),
  Diff1 = gb_sets:subtract(FeasibleTags, AddedTags),
  Diff2 = gb_sets:subtract(AddedTags, FeasibleTags),
  NoDiff = gb_sets:is_empty(Diff1) andalso gb_sets:is_empty(Diff2),
  [{"added tags match collected tags", ?_assertEqual(true, NoDiff)}].

setup_for_tags(M) ->
  Cache = ets:new(M, [ordered_set, protected]),
  cuter_codeserver:init_branch_counter(),
  {M, Cache}.

cleanup_for_tags({_, Cache}) ->
  ets:delete(Cache).

%%====================================================================
%% Helper functions
%%====================================================================

setup(M) ->
  MDb = ets:new(M, [ordered_set, protected]),
  {M, MDb}.

cleanup({_M, MDb}) ->
  ets:delete(MDb).

%% -------------------------------------------------------------------
%% kfun API
%% -------------------------------------------------------------------

-spec construct_and_access_kfun_test() -> ok.
construct_and_access_kfun_test() ->
  Code = cerl:c_fun([cerl:c_nil()], cerl:c_nil()),
  Kfun = cuter_cerl:kfun(Code, true),
  GotCode = cuter_cerl:kfun_code(Kfun),
  GotIsExported = cuter_cerl:kfun_is_exported(Kfun),
  [?assertEqual(Code, GotCode), ?assertEqual(true, GotIsExported)].
