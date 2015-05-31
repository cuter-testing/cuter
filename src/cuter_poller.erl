%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_poller).

-export([start/6, poll/1]).

%% The configuration of the poller.
-record(conf, {
  codeServer    :: pid(),
  mod           :: cuter:mod(),
  func          :: atom(),
  dataDir       :: file:filename(),
  depth         :: cuter:depth(),
  scheduler     :: pid()
}).
-type configuration() :: #conf{}.

-define(SLEEP, 300).

%% ------------------------------------------------------------------
%% Main functions of the poller
%% ------------------------------------------------------------------

-spec start(pid(), pid(), cuter:mod(), atom(), file:filename(), cuter:depth()) -> pid().
start(CodeServer, Scheduler, M, F, Dir, Depth) ->
  Conf = mk_conf(CodeServer, Scheduler, M, F, Dir, Depth),
  spawn_link(?MODULE, poll, [Conf]).

-spec poll(configuration()) -> ok.
poll(Conf) ->
  process_flag(trap_exit, true),
  loop(Conf).

-spec loop(configuration()) -> ok.
loop(Conf) ->
  Scheduler = Conf#conf.scheduler,
  case cuter_scheduler_maxcover:request_input(Scheduler) of
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
          ok = cuter_scheduler_maxcover:store_execution(Scheduler, Handle, Info),
          loop(Conf)
      end
  end.

-spec stop() -> ok.
stop() ->
  ok.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

-spec concolic_execute(configuration(), cuter_scheduler_maxcover:handle(), cuter:input()) -> cuter_analyzer:info() | cuter_error.
concolic_execute(Conf, Handle, Input) ->
  cuter_pp:input(Handle, Input),
  BaseDir = Conf#conf.dataDir,
  DataDir = cuter_lib:get_data_dir(BaseDir, Handle),
  TraceDir = cuter_lib:get_trace_dir(DataDir),  % Directory to store process traces
  IServer = cuter_iserver:start(Conf#conf.mod, Conf#conf.func, Input, TraceDir, Conf#conf.depth, Conf#conf.codeServer),
  retrieve_info(IServer, Handle, DataDir).

-spec retrieve_info(pid(), cuter_scheduler_maxcover:handle(), file:filename()) -> cuter_analyzer:info() | cuter_error.
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

-spec wait_for_execution(pid()) -> {ok, cuter_iserver:execution_status(), cuter_iserver:logs()} | {error, any()}.
wait_for_execution(IServer) ->
  receive
    {IServer, ExStatus, Logs} ->
      ok = wait_for_iserver(IServer),
      {ok, ExStatus, Logs};
    {'EXIT', IServer, Why} ->
      {error, Why}
  end.

-spec wait_for_iserver(pid()) -> ok | not_ok.
wait_for_iserver(IServer) ->
  receive {'EXIT', IServer, normal} -> ok
  after 10000 -> not_ok
  end.

%% ----------------------------------------------------------------------------
%% Manage the configuration.
%% ----------------------------------------------------------------------------

-spec mk_conf(pid(), pid(), cuter:mod(), atom(), file:filename(), cuter:depth()) -> configuration().
mk_conf(CodeServer, Scheduler, M, F, Dir, Depth) ->
  #conf{ codeServer = CodeServer
       , mod = M
       , func = F
       , dataDir = Dir
       , depth = Depth
       , scheduler = Scheduler}.
