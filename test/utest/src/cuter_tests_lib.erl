%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests_lib).

-include("include/eunit_config.hrl").

-export([setup_dir/0, get_python_command/0, get_module_attrs/2]).

%% Create a directory for temporary use
-spec setup_dir() -> file:filename_all().
setup_dir() ->
  {ok, CWD} = file:get_cwd(),
  cuter_lib:get_tmp_dir(CWD).

-spec get_python_command() -> string().
get_python_command() ->
  PyFile = filename:absname("cuter_interface.py", ?PRIV),
  ?PYTHON_PATH ++ " -u " ++ PyFile.

-spec get_module_attrs(atom(), boolean()) -> [cuter_cerl:cerl_attr_type()].
get_module_attrs(Module, WithPmatch) ->
  Beam = code:which(Module),
  {ok, {Module, [{abstract_code, {_, AbstractCode}}]}} = beam_lib:chunks(Beam, [abstract_code]),
  {ok, Module, AST} = compile:forms(AbstractCode, compile_options(WithPmatch)),
  cerl:module_attrs(AST).

compile_options(true) -> [to_core, {core_transform, cerl_pmatch}];
compile_options(false) -> [to_core].
