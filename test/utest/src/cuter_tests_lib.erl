%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests_lib).

-include("include/eunit_config.hrl").

-export([setup_dir/0, get_python_command/0]).

%% Create a directory for temporary use
-spec setup_dir() -> file:filename_all().
setup_dir() ->
  {ok, CWD} = file:get_cwd(),
  cuter_lib:get_tmp_dir(CWD).

-spec get_python_command() -> string().
get_python_command() ->
  PyFile = filename:absname("cuter_interface.py", ?PRIV),
  ?PYTHON_PATH ++ " -u " ++ PyFile.
