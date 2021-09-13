%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/cuter_macros.hrl").

-spec test() -> ok | {error, any()}.  %% This should be provided by EUnit

-define(MODS_LIST, [lists, dict, orddict, ets, os, string, filelib, beam_lib, cerl]).

-spec load_modules_test_() -> any().
load_modules_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(),
    As = [load_mod(Cs, M) || M <- ?MODS_LIST],
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"load modules", As}]
  end,
  {"Just query modules once", {setup, Setup, Cleanup, Inst}}.

load_mod(Cs, M) ->
  Rep = cuter_codeserver:load(Cs, M),
  {atom_to_list(M), ?_assertMatch({ok, _}, Rep)}.


-spec load_and_retrieve_test_() -> any().
load_and_retrieve_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(),
    R1 = cuter_codeserver:load(Cs, lists),
    R2 = cuter_codeserver:load(Cs, os),
    R3 = cuter_codeserver:load(Cs, lists),
    R4 = cuter_codeserver:load(Cs, os),
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"query modules", ?_assertMatch({{ok,X}, {ok,Y}, {ok,X}, {ok,Y}}, {R1, R2, R3, R4})}]
  end,
  {"Query modules multiple times", {setup, Setup, Cleanup, Inst}}.


-spec get_mfa_spec_test_() -> any().
get_mfa_spec_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(),
    %% types_and_specs:foo/0
    Spec0 = cuter_codeserver:retrieve_spec(Cs, {types_and_specs, foo, 0}),
    Expected0 = {[cuter_types:t_function([], cuter_types:t_atom_lit(ok))], []},
    %% types_and_specs:f11/1, f12/1
    Spec11 = cuter_codeserver:retrieve_spec(Cs, {types_and_specs, f11, 1}),
    Spec12 = cuter_codeserver:retrieve_spec(Cs, {types_and_specs, f12, 1}),
    T1 = cuter_types:unique_type_name(types_and_specs, t6, []),
    Expected1 = [cuter_types:t_function([cuter_types:t_userdef(T1)], cuter_types:t_float())],
    Stop = cuter_codeserver:stop(Cs),
    [ {"types_and_specs:foo/0", ?_assertEqual({ok, Expected0}, Spec0)}
    , {"types_and_specs:f11/1", ?_assertEqual(Expected1, element(1, element(2, Spec11)))}
    , {"types_and_specs:f12/1", ?_assertEqual(Expected1, element(1, element(2, Spec12)))}
    , {"codeserver termination", ?_assertEqual(ok, Stop)}
    ]
  end,
  {"Get specs of mfas", {setup, Setup, Cleanup, Inst}}.

-spec error_load_test_() -> term().
error_load_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(),
    R1 = cuter_codeserver:load(Cs, erlang),
    R2 = cuter_codeserver:load(Cs, foobar),
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"query invalid modules", ?_assertEqual({{error,preloaded}, {error, non_existing}}, {R1, R2})}]
  end,
  {"Query invalid modules", {setup, Setup, Cleanup, Inst}}.

%% ===================================================================
%% Helper functions
%% ===================================================================

setup() ->
  cuter_config:start(),
  cuter_config:store(?DISABLE_PMATCH, true),
  cuter_config:store(?DISABLE_TYPE_NORMALIZATION, false),
  cuter_config:store(?VERBOSITY_LEVEL, cuter_pp:default_reporting_level()),
  cuter_config:store(?WHITELISTED_MFAS, cuter_mock:empty_whitelist()),
  ok = cuter_pp:start(),
  {}.

cleanup(_) ->
  cuter_pp:stop(),
  cuter_config:stop(),
  timer:sleep(200),
  ok.
