%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_iserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-define(Pmatch, true).

-type descr() :: nonempty_string().

%% Ensure start/stop runs properly
-spec start_stop_test_() -> [{descr(), _}, ...].
start_stop_test_() ->
  {_Dir, Server} = Start = setup(),
  R = receive
        {Server, ExStatus, _Result} -> ExStatus
      end,
  Stop = cleanup(Start),
  [{"Start & Finish", ?_assertEqual(ok, Stop)},
   {"Expected Result", ?_assertMatch({success, {_, [17,42], _}}, R)}].

setup() ->
  process_flag(trap_exit, true),
  Dir = cuter_tests_lib:setup_dir(),
  ok = cuter_pp:start(cuter_pp:default_reporting_level()),
  CodeServer = cuter_codeserver:start(self(), ?Pmatch, cuter_mock:empty_whitelist(), true),
  Server = cuter_iserver:start(lists, reverse, [[42,17]], Dir, ?TRACE_DEPTH, CodeServer),
  {Dir, Server}.

cleanup({Dir, Server}) ->
  receive
    {'EXIT', Server, normal} -> ok
  after
    5000 -> ok
  end,
  cuter_pp:stop(),
  timer:sleep(200),
  cuter_lib:clear_and_delete_dir(Dir).

