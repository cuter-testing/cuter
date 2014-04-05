%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_iserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning


%% Ensure start/stop runs properly
-spec start_stop_test_() -> any().
start_stop_test_() ->
  {_Dir, Server} = Start = setup(),
  R = 
    receive
      {Server, ExStatus, _Result} -> ExStatus
    end,
  Stop = cleanup(Start),
  [{"Start & Finish", ?_assertEqual(ok, Stop)},
   {"Expected Result", ?_assertEqual({success, {true, true}}, R)}].

setup() ->
  process_flag(trap_exit, true),
  Dir = cuter_tests_lib:setup_dir(),
  Server = cuter_iserver:start(erlang, is_integer, [42], Dir, Dir, ?TRACE_DEPTH),
  {Dir, Server}.

cleanup({Dir, Server}) ->
  receive
    {'EXIT', Server, normal} -> ok
  after
    5000 -> ok
  end,
  cuter_tests_lib:cleanup_dir(Dir).

