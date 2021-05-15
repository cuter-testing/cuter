%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_iserver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").
-include_lib("include/cuter_macros.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

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
  cuter_config:start(),
  cuter_config:store(?DISABLE_PMATCH, false),
  cuter_config:store(?DISABLE_TYPE_NORMALIZATION, false),
  cuter_config:store(?VERBOSITY_LEVEL, cuter_pp:default_reporting_level()),
  cuter_config:store(?WHITELISTED_MFAS, cuter_mock:empty_whitelist()),
  ok = cuter_pp:start(),
  CodeServer = cuter_codeserver:start(self()),
  Server = cuter_iserver:start(lists, reverse, [[42,17]], Dir, ?TRACE_DEPTH, CodeServer),
  {Dir, Server}.

cleanup({Dir, Server}) ->
  receive
    {'EXIT', Server, normal} -> ok
  after
    5000 -> ok
  end,
  cuter_pp:stop(),
  cuter_config:stop(),
  timer:sleep(200),
  cuter_lib:clear_and_delete_dir(Dir).

