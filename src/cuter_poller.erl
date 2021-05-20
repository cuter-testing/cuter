%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_poller).

-export([start/6, send_stop_message/1]).

%% The configuration of the poller.
-record(conf, {
  %% The handle for the code server.
  codeServer :: cuter_codeserver:codeserver(),
  %% The module part of the MFA entry point.
  m :: cuter:mod(),
  %% The function part of the MFA entry point.
  f :: cuter:func(),
  %% The base directory to store execution traces.
  dir :: file:filename(),
  %% The depth for tracing.
  depth :: cuter:depth(),
  %% The handle for the scheduler.
  scheduler :: cuter_scheduler:scheduler()
}).
-type configuration() :: #conf{}.

-define(SLEEP, 50).

-type poller() :: pid().

%% ------------------------------------------------------------------
%% Main functions of the poller
%% ------------------------------------------------------------------

-spec start(cuter_codeserver:codeserver(), cuter_scheduler:scheduler(), cuter:mod(),
            cuter:func(), file:filename(), cuter:depth()) -> poller().
start(CodeServer, Scheduler, M, F, Dir, Depth) ->
  Conf = #conf{ codeServer = CodeServer
              , m = M
              , f = F
              , dir = Dir
              , depth = Depth
              , scheduler = Scheduler },
  spawn_link(fun() -> poll(Conf) end).

-spec poll(configuration()) -> ok.
poll(Conf) ->
  process_flag(trap_exit, true),
  loop(Conf).

-spec loop(configuration()) -> ok.
loop(#conf{ scheduler = Scheduler } = Conf) ->
  case got_stop_message() of
    true -> stop();
    false ->
      case cuter_scheduler:request_input(Scheduler) of
        %% No input is or will be available in the future.
        empty ->
          stop();
        %% No input is currently available.
        try_later ->
          timer:sleep(?SLEEP),
          loop(Conf);
        %% Got an input to run.
        {Handle, As} ->
          case concolic_execute(Conf, Handle, As) of
            cuter_error ->
              stop();
            Info ->
              ok = cuter_scheduler:store_execution(Scheduler, Handle, Info),
              loop(Conf)
          end
      end
  end.

-spec stop() -> ok.
stop() ->
  ok.

%% Stops a poller process.
-spec send_stop_message(poller()) -> ok.
send_stop_message(Poller) ->
  io:format("Stopping poller ~p...~n", [Poller]),
  Poller ! stop,
  ok.

%% Checks if the poller process should stop.
-spec got_stop_message() -> boolean().
got_stop_message() ->
  receive stop -> true
  after 0 -> false
  end.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

-spec concolic_execute(configuration(), cuter_scheduler:handle(), cuter:input()) -> cuter_analyzer:info() | cuter_error.
concolic_execute(#conf{ dir = Dir, m = M, f = F, depth = D, codeServer = CS }, Handle, Input) ->
  cuter_pp:input(Handle, Input),
  DataDir = cuter_lib:get_data_dir(Dir, Handle),
  %% Directory to store the traces of a process.
  TraceDir = cuter_lib:get_trace_dir(DataDir),
  IServer = cuter_iserver:start(M, F, Input, TraceDir, D, CS),
  retrieve_info(IServer, Handle, DataDir).

-spec retrieve_info(cuter_iserver:iserver(), cuter_scheduler:handle(), file:filename()) -> cuter_analyzer:info() | cuter_error.
retrieve_info(IServer, Handle, DataDir) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Logs} ->
      cuter_pp:execution_status(Handle, ExStatus),
      cuter_pp:execution_info(Handle, Logs),
      case cuter_analyzer:get_result(ExStatus) of
        internal_error -> cuter_error;
        ExResult ->
          Mappings = cuter_analyzer:get_mapping(Logs),
          Traces = cuter_analyzer:get_traces(Logs),
          Int = cuter_analyzer:get_int_process(Logs),
          RawInfo = cuter_analyzer:mk_raw_info(Mappings, ExResult, Traces, Int, DataDir),
          AnalyzedInfo = cuter_analyzer:process_raw_execution_info(RawInfo),
          cuter_pp:path_vertex(Handle, cuter_analyzer:pathVertex_of_info(AnalyzedInfo)),
          cuter_pp:flush(Handle),
          AnalyzedInfo
      end;
    {error, Why} ->
      R = {internal_error, {iserver, node(), Why}},
      cuter_pp:execution_status(Handle, R),
      cuter_pp:flush(Handle),
      cuter_error
  end.

-spec wait_for_execution(cuter_iserver:iserver()) -> {ok, cuter_iserver:execution_status(), cuter_iserver:logs()} 
                                                   | {error, any()}.
wait_for_execution(IServer) ->
  receive
    {IServer, ExStatus, Logs} ->
      ok = wait_for_iserver(IServer),
      {ok, ExStatus, Logs};
    {'EXIT', IServer, Why} ->
      {error, Why}
  end.

-spec wait_for_iserver(cuter_iserver:iserver()) -> ok | not_ok.
wait_for_iserver(IServer) ->
  receive {'EXIT', IServer, normal} -> ok
  after 10000 -> not_ok
  end.
