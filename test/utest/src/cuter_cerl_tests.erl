%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").
-include("include/cuter_macros.hrl").

-spec test() -> ok | {error, any()}.

-spec load_lists_module_test() -> any().
load_lists_module_test() ->
  TagGen = fun() -> {?BRANCH_TAG_PREFIX, 42} end,
  {ok, Klists} = cuter_cerl:load(lists, TagGen, false),
  R = cuter_cerl:destroy_kmodule(Klists),
  ?_assertEqual(ok, R).

-spec exported_mfas_in_lists_module_test_() -> any().
exported_mfas_in_lists_module_test_() ->
  TagGen = fun() -> {?BRANCH_TAG_PREFIX, 42} end,
  {ok, Klists} = cuter_cerl:load(lists, TagGen, false),
  MfaKfuns = kfuns_from_exports(lists, Klists),
  Assertions = [{mfa_to_string(Mfa), assert_is_exported(Kfun)} || {Mfa, Kfun} <- MfaKfuns],
  R = cuter_cerl:destroy_kmodule(Klists),
  Assertions ++ [?_assertEqual(ok, R)].

mfa_to_string({M, F, A}) ->
  atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).

kfuns_from_exports(M, Kmodule) ->
  Mfas = [{M, F, A} || {F, A} <- M:module_info(exports)],
  Fn = fun(Mfa) ->
      {ok, Kfun} = cuter_cerl:kmodule_kfun(Kmodule, Mfa),
      {Mfa, Kfun}
    end,
  [Fn(Mfa) || Mfa <- Mfas].

assert_is_exported(Kfun) ->
  ?_assertEqual(true, cuter_cerl:kfun_is_exported(Kfun)).

-spec generate_and_collect_tags_for_lists_module_test() -> any().
generate_and_collect_tags_for_lists_module_test() ->
  cuter_codeserver:init_branch_counter(),
  TagGen = fun cuter_codeserver:generate_tag/0,
  {ok, Klists} = cuter_cerl:load(lists, TagGen, false),
  FoldFn = fun(Elem, Acc) ->
      case Elem of
        {{_,_,_}, Kfun} ->
          Code = cuter_cerl:kfun_code(Kfun),
          Tags = cuter_cerl:collect_feasible_tags(Code, all),
          gb_sets:union(Acc, Tags);
        _ -> Acc
      end
    end,
  FeasibleTags = ets:foldl(FoldFn, gb_sets:new(), Klists),
  N = cuter_codeserver:get_branch_counter(),
  AddedTags = gb_sets:from_list(lists:seq(1, N)),
  Diff1 = gb_sets:subtract(FeasibleTags, AddedTags),
  Diff2 = gb_sets:subtract(AddedTags, FeasibleTags),
  NoDiff = gb_sets:is_empty(Diff1) andalso gb_sets:is_empty(Diff2),
  R = cuter_cerl:destroy_kmodule(Klists),
  [?_assertEqual(true, NoDiff), ?_assertEqual(ok, R)].

%% -------------------------------------------------------------------
%% kfun API
%% -------------------------------------------------------------------

-spec construct_and_access_kfun_test() -> any.
construct_and_access_kfun_test() ->
  Code = cerl:c_fun([cerl:c_nil()], cerl:c_nil()),
  Kfun = cuter_cerl:kfun(Code, true),
  GotCode = cuter_cerl:kfun_code(Kfun),
  GotIsExported = cuter_cerl:kfun_is_exported(Kfun),
  [?assertEqual(Code, GotCode), ?assertEqual(true, GotIsExported)].

%% -------------------------------------------------------------------
%% kmodule API
%% -------------------------------------------------------------------

-spec construct_and_access_kmodule_test() -> any.
construct_and_access_kmodule_test() ->
  Code = cerl:c_fun([cerl:c_var(0)], cerl:c_var(0)),
  Kfun = cuter_cerl:kfun(Code, true),
  SomeMod = some_module,
  SomeModAtm = cerl:c_atom(SomeMod),
  F1Var  = cerl:c_var({f,1})
  MI0Var = cerl:c_var({module_info,0}),
  MI1Var = cerl:c_var({module_info,1}),
  ErlangAtm = cerl:c_atom(erlang),
  GMIAtm = cerl:c_atom(get_module_info),
  AST = cerl:c_module(
	  cerl:c_atom(SomeMod),
	  [F1Var, MI0Var, MI1Var],
	  [{cerl:c_atom(file),
	    cerl:abstract([{"some_module.erl",1}])}],
	  [{F1Var,
	    cerl:c_fun([cerl:c_var(0)], cerl:c_var(0))},
	   {MI0Var,
	    cerl:c_fun([],
		       cerl:c_call(ErlangAtm,
				   GMIAtm,
				   [SomeModAtm]))},
	   {MI1Var,
	    cerl:c_fun([cerl:c_var(0)],
		       cerl:c_call(ErlangAtm,
				   GMIAtm,
				   [SomeModAtm, cerl:c_var(0)]))}]),
  Kmodule = cuter_cerl:kmodule(SomeMod, AST, fun() -> ok end),
  GotKfun = cuter_cerl:kmodule_kfun(Kmodule, {SomeMod, f, 1}),
  RS = cuter_cerl:kmodule_specs(Kmodule),
  RT = cuter_cerl:kmodule_types(Kmodule),
  R = cuter_cerl:destroy_kmodule(Kmodule),
  [?_assertEqual(nil, RS), ?_assertEqual(nil, RT),
   ?assertEqual({ok, Kfun}, GotKfun), ?_assertEqual(ok, R)].
