%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests_lib).

-include("eunit_config.hrl").

-export([setup_dir/0]).

%% Create a directory for temporary use
-spec setup_dir() -> file:filename_all().
setup_dir() ->
  {ok, CWD} = file:get_cwd(),
  TmpDir = cuter_lib:get_tmp_dir(CWD),
  CoreDir = cuter_lib:get_core_dir(TmpDir),
  ok = filelib:ensure_dir(CoreDir),
  TmpDir.

