%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests_lib).

-include("include/eunit_config.hrl").

-export([setup_dir/0, get_python_command/0, get_module_attrs/2, sample_trace_file/1]).

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

%% Create a sample trace file.
-spec sample_trace_file(file:name()) -> ok.
sample_trace_file(Fname) ->
  As = [1, 2],
  put('__conc_depth', 100),
  % Create the logfile
  {ok, Fd} = cuter_log:open_file(Fname, write),
  % Abstract the input
  {[P1, P2], Mapping} = cuter_symbolic:abstract(As),
  % Log the commands.
  cuter_log:log_symb_params(Fd, Mapping),
  cuter_log:log_equal(Fd, true, P1, 1, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, false, P2, 42, cuter_cerl:empty_tag()),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_plus, 2}, [P1, P2], X),
  cuter_log:log_equal(Fd, false, X, 45, cuter_cerl:empty_tag()),
  % Close the logfile
  cuter_log:close_file(Fd).
