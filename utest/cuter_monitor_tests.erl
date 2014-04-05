%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_monitor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning


%% Ensure start/stop runs properly
-spec start_stop_test_() -> any().
start_stop_test_() ->
  Start = setup(),
  Stop = cleanup(Start),
  {"Start & Stop", ?_assertEqual(ok, Stop)}.

%% Validate subscriber processes
-spec validate_subscribers_test_() -> any().
validate_subscribers_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Inst = fun(N) ->
    fun ({_, MonitorServer}) -> validate_subscribers(MonitorServer, N) end
  end,
  Title = fun(N) -> "Validate subscribers. Number of Processes: " ++ integer_to_list(N) end,
  [{Title(N), {setup, Setup, Cleanup, Inst(N)}} || N <- [10, 100, 500, 700, 1000]].

setup() ->
  Dir = cuter_tests_lib:setup_dir(),
  MonitorServer = cuter_monitor:start(Dir, self(), ?TRACE_DEPTH, ?EXEC_PREFIX),
  {Dir, MonitorServer}.

cleanup({Dir, MonitorServer}) ->
  ok = cuter_monitor:stop(MonitorServer),
  cuter_tests_lib:cleanup_dir(Dir).

validate_subscribers(MonitorServer, N) ->
  process_flag(trap_exit, true),
  Me = self(),
  Pids = [spawn_link(fun() -> validate_subscriber(MonitorServer, Me) end) || _ <- lists:seq(1, N)],
  Fds = [receive {Pid, check_fd, X} -> X end || Pid <- Pids],
  Mons = [receive {Pid, is_monitored, X} -> X end || Pid <- Pids],
  
  lists:foreach(fun(Pid) -> Pid ! {Me, stop} end, Pids),
  lists:foreach(fun(Pid) -> receive {'EXIT', Pid, normal} -> ok end end, Pids),
  [{"Validate file descriptors", ?_assertEqual(true, lists:all(fun(X) -> X end, Fds))},
   {"Validate monitors", ?_assertEqual(true, lists:all(fun(X) -> X end, Mons))}].

validate_subscriber(MonitorServer, Parent) ->
  {ok, X} = cuter_monitor:subscribe(MonitorServer, Parent),
  Y = cuter_monitor:file_descriptor(MonitorServer),
  Parent ! {self(), check_fd, X =:= Y},
  M = cuter_monitor:is_monitored(MonitorServer, self()),
  Parent ! {self(), is_monitored, M},
  receive {Parent, stop} -> ok end.

