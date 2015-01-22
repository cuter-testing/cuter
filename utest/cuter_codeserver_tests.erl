%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning


%% Load modules
-spec load_modules_test_() -> term().
load_modules_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(Dir) ->
    Cs = cuter_codeserver:start(Dir, self()),
    As = [load_mod(Cs, M) || M <- ?MODS_LIST],
    Stop = cuter_codeserver:stop(Cs),
    Chk = is_dir_empty(Cs, Dir),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"clean up .core directory", ?_assertEqual(true, Chk)},
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
  Inst = fun(Dir) ->
    Cs = cuter_codeserver:start(Dir, self()),
    R1 = cuter_codeserver:load(Cs, lists),
    R2 = cuter_codeserver:load(Cs, os),
    R3 = cuter_codeserver:load(Cs, lists),
    R4 = cuter_codeserver:load(Cs, os),
    Stop = cuter_codeserver:stop(Cs),
    Chk = is_dir_empty(Cs, Dir),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"clean up .core directory", ?_assertEqual(true, Chk)},
     {"query modules", ?_assertMatch({{ok,X}, {ok,Y}, {ok,X}, {ok,Y}}, {R1, R2, R3, R4})}]
  end,
  {"Query modules multiple times", {setup, Setup, Cleanup, Inst}}.

%% Erroneous modules
-spec error_load_test_() -> term().
error_load_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(Dir) ->
    Cs = cuter_codeserver:start(Dir, self()),
    R1 = cuter_codeserver:load(Cs, erlang),
    R2 = cuter_codeserver:load(Cs, foobar),
    Stop = cuter_codeserver:stop(Cs),
    Chk = is_dir_empty(Cs, Dir),
    [{"codeserver termination", ?_assertEqual(ok, Stop)},
     {"clean up .core directory", ?_assertEqual(true, Chk)},
     {"query invalid modules", ?_assertEqual({{error,preloaded}, {error, non_existing}}, {R1, R2})}]
  end,
  {"Query invalid modules", {setup, Setup, Cleanup, Inst}}.


%%====================================================================
%% Helper functions
%%====================================================================

is_dir_empty(CodeServer, Dir) ->
  receive {'EXIT', CodeServer, normal} -> ok
  after 100 -> ok
  end,
  case file:list_dir(Dir) of
    {ok, Fs} -> length(Fs) =:= 0;
    {error, enoent} -> true
  end.

setup() ->
  meck:new(cuter_iserver),
  meck:expect(cuter_iserver, code_logs, fun(_, _) -> ok end),
  cuter_tests_lib:setup_dir().

cleanup(Dir) ->
  meck:unload(cuter_iserver),
  cuter_lib:clear_and_delete_dir(Dir).
