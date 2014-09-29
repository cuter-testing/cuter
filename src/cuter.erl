%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/4]).

-include("cuter_macros.hrl").

-spec run(module(), atom(), [any()], pos_integer()) -> ok.
run(M, F, As, Depth) ->
  Conf = initialize_app(M, F, As, Depth),
  _ = concolic_execute(Conf, As),
  cuter_lib:clear_and_delete_dir(maps:get(dataDir, Conf)),
  ok.

-spec initialize_app(atom(), atom(), [any()], integer()) -> map().
initialize_app(M, F, As, Depth) ->
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% Disable error_logger
  cuter_pp:mfa(M, F, length(As)),
  #{mod => M, func => F, dataDir => ?TMP_DIR, depth => Depth, no => 1}.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

concolic_execute(Conf, Input) ->
  cuter_pp:input(Input),
  DataDir = ?DATA_DIR(maps:get(dataDir, Conf), maps:get(no, Conf)),
  CoreDir = ?CORE_DIR(DataDir),    %% Directory to store .core files
  TraceDir = ?TRACE_DIR(DataDir),  %% Directory to store traces
  IServer = cuter_iserver:start(maps:get(mod, Conf), maps:get(func, Conf), Input, CoreDir, TraceDir, maps:get(depth, Conf)),
  Resp = retrieve_result(IServer),
  #{dir => DataDir, resp => Resp}.

-spec retrieve_result(pid()) -> cuter_iserver:execution_status().
retrieve_result(IServer) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Info}=R ->
      cuter_pp:exec_status(ExStatus),
      cuter_pp:exec_info(Info),
      R;
    {error, Why} ->
      R = {internal_error, iserver, node(), Why},
      cuter_pp:exec_status(R),
      R
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
