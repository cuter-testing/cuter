%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/4, run/5, run_once/4, run_once/5]).

-include("cuter_macros.hrl").

-spec run_once(module(), atom(), [any()], pos_integer()) -> ok.
run_once(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run_once(M, F, As, Depth, CWD).

-spec run_once(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run_once(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir),
  Info = concolic_execute(Conf, As),
  ok = cuter_scheduler:seed_execution(maps:get(scheduler, Conf), Info),
  stop(Conf),
  cuter_lib:clear_and_delete_dir(maps:get(dataDir, Conf)),
  ok.

-spec run(module(), atom(), [any()], pos_integer()) -> ok.
run(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run(M, F, As, Depth, CWD).

-spec run(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir),
  Info = concolic_execute(Conf, As),
  ok = cuter_scheduler:seed_execution(maps:get(scheduler, Conf), Info),
  loop(Conf).

loop(Conf) ->
  case cuter_scheduler:request_input(maps:get(scheduler, Conf)) of
    empty -> stop(Conf);
    {Ref, As} ->
      No = maps:get(no, Conf) + 1,
      Conf_n = Conf#{no := No},
      Info = concolic_execute(Conf_n, As),
      ok = cuter_scheduler:store_execution(maps:get(scheduler, Conf), Ref, Info),
      loop(Conf_n)
  end.

-spec initialize_app(atom(), atom(), [any()], integer(), file:filename_all()) -> map().
initialize_app(M, F, As, Depth, BaseDir) ->
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% Disable error_logger
  cuter_pp:mfa(M, F, length(As)),
  Sched = cuter_scheduler:start(?PYTHON_CALL, Depth),
  #{mod => M, func => F, dataDir => cuter_lib:get_tmp_dir(BaseDir), depth => Depth, no => 1, scheduler => Sched}.

stop(Conf) ->
  _ = file:del_dir(filename:absname(maps:get(dataDir, Conf))),
  cuter_scheduler:stop(maps:get(scheduler, Conf)),
  ok.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

concolic_execute(Conf, Input) ->
  cuter_pp:input(Input),
  DataDir = cuter_lib:get_data_dir(maps:get(dataDir, Conf), maps:get(no, Conf)),
  CoreDir = cuter_lib:get_core_dir(DataDir),    % Directory to store .core files
  TraceDir = cuter_lib:get_trace_dir(DataDir),  % Directory to store traces
  IServer = cuter_iserver:start(maps:get(mod, Conf), maps:get(func, Conf), Input, CoreDir, TraceDir, maps:get(depth, Conf)),
  case retrieve_result(IServer) of
    cuter_error -> stop(Conf);
    Info -> Info#{dir => DataDir}
  end.

-spec retrieve_result(pid()) -> {[cuter_symbolic:mapping()], [cuter_analyzer:node_trace()]} | cuter_error.
retrieve_result(IServer) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Info} ->
      cuter_pp:exec_status(ExStatus),
      cuter_pp:exec_info(Info),
      case cuter_analyzer:get_result(ExStatus) of
        internal_error -> cuter_error;
        _ ->
          Ms = cuter_analyzer:get_mapping(Info),
          Ts = cuter_analyzer:get_traces(Info),
          Int = cuter_analyzer:get_int(Info),
          #{mappings => Ms, traces => Ts, int => Int}
      end;
    {error, Why} ->
      R = {internal_error, iserver, node(), Why},
      cuter_pp:exec_status(R),
      cuter_error
  end.

wait_for_execution(IServer) ->
  receive
    {IServer, ExStatus, Info} ->
      ok = wait_for_iserver(IServer),
      {ok, ExStatus, Info};
    {'EXIT', IServer, Why} ->
      {error, Why}
  end.

wait_for_iserver(IServer) ->
receive
  {'EXIT', IServer, normal} -> ok
after
  10000 -> not_ok
end.

