%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/cuter_macros.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-type descr() :: nonempty_string().
-type f_one() :: fun((term()) -> term()).
-type setup() :: {'setup', fun(() -> term()), f_one(), f_one()}.
-type ret_t() :: {descr(), setup()}.

-spec load_mfas_test_() -> ret_t().
load_mfas_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(_) ->
    Cs = cuter_codeserver:start(),
    Mfas = [{lists, reverse, 1}, {dict, to_list, 1}, {orddict, new, 0},
      {ets, lookup, 2}, {os, cmd, 1}, {string, chr, 2}, {filelib, find_file, 2},
      {beam_lib, chunks, 2}, {cerl, module_attrs, 1}],
    As = [load_mfa(Cs, Mfa) || Mfa <- Mfas],
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"load MFAs", As}]
  end,
  {"load MFAs", {setup, Setup, Cleanup, Inst}}.

load_mfa(Cs, {M, F, A}=Mfa) ->
  Rep = cuter_codeserver:mfa_code(Cs, Mfa),
  {atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A), ?_assertMatch({ok, _}, Rep)}.

%% Get the specs of mfas.
-spec get_spec_test_() -> ret_t().
get_spec_test_() ->
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
    R1 = cuter_codeserver:mfa_code(Cs, {erlang, atom_to_list, 1}),
    R2 = cuter_codeserver:mfa_code(Cs, {foo, bar, 1}),
    Stop = cuter_codeserver:stop(Cs),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"query invalid modules", ?_assertEqual({error, error}, {R1, R2})}]
  end,
  {"Query invalid modules", {setup, Setup, Cleanup, Inst}}.


%%====================================================================
%% Helper functions
%%====================================================================

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
