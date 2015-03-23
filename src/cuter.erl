%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/4, run/5, run_once/4, run_once/5]).

-include("include/cuter_macros.hrl").

-type configuration() :: #{mod => module(),
                           func => atom(),
                           dataDir => file:filename_all(),
                           depth => pos_integer(),
                           no => integer(),
                           scheduler => pid()}.



-spec run_once(module(), atom(), [any()], pos_integer()) -> ok.
run_once(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run_once(M, F, As, Depth, CWD).

-spec run_once(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run_once(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir),
  run_seed_execution(Conf, As, false).

-spec run(module(), atom(), [any()], pos_integer()) -> ok.
run(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run(M, F, As, Depth, CWD).

-spec run(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir),
  run_seed_execution(Conf, As, true).

-spec run_seed_execution(configuration(), [any()], boolean()) -> ok.
run_seed_execution(Conf, As, Loop) ->
  case concolic_execute(Conf, As) of
    cuter_error ->
      stop(Conf);
    Info ->
      ok = cuter_scheduler:seed_execution(maps:get(scheduler, Conf), Info),
      run_loop(Loop, Conf)
  end.

-spec run_loop(boolean(), configuration()) -> ok.
run_loop(true, Conf) ->
  loop(Conf);
run_loop(false, Conf) ->
%  io:format("[&&] ~p~n", [Conf]),
  stop(Conf).

-spec loop(configuration()) -> ok.
loop(Conf) ->
  case cuter_scheduler:request_input(maps:get(scheduler, Conf)) of
    empty -> stop(Conf);
    {Ref, As} ->
      No = maps:get(no, Conf) + 1,
      Conf_n = Conf#{no := No},
      case concolic_execute(Conf_n, As) of
        cuter_error ->
          stop(Conf);
        Info ->
          ok = cuter_scheduler:store_execution(maps:get(scheduler, Conf), Ref, Info),
          loop(Conf_n)
      end
  end.

-spec initialize_app(module(), atom(), [any()], pos_integer(), file:filename_all()) -> configuration().
initialize_app(M, F, As, Depth, BaseDir) ->
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% Disable error_logger
  cuter_pp:mfa(M, F, length(As)),
  Sched = cuter_scheduler:start(?PYTHON_CALL, Depth),
  #{mod => M,
    func => F,
    no => 1,
    depth => Depth,
    dataDir => cuter_lib:get_tmp_dir(BaseDir),
    scheduler => Sched}.

-spec stop(configuration()) -> ok.
stop(Conf) ->
  cuter_scheduler:stop(maps:get(scheduler, Conf)),
  cuter_lib:clear_and_delete_dir(maps:get(dataDir, Conf)).

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

-spec concolic_execute(configuration(), [any()]) -> cuter_analyzer:info() | cuter_error.
concolic_execute(Conf, Input) ->
  cuter_pp:input(Input),
  BaseDir = maps:get(dataDir, Conf),
  DataDir = cuter_lib:get_data_dir(BaseDir, maps:get(no, Conf)),
  CoreDir = cuter_lib:get_core_dir(DataDir),    % Directory to store Core Erlang files
  TraceDir = cuter_lib:get_trace_dir(DataDir),  % Directory to store process traces
  M = maps:get(mod, Conf),
  F = maps:get(func, Conf),
  Depth = maps:get(depth, Conf),
  IServer = cuter_iserver:start(M, F, Input, CoreDir, TraceDir, Depth),
  retrieve_info(IServer, DataDir).

-spec retrieve_info(pid(), file:filename_all()) -> cuter_analyzer:info() | cuter_error.
retrieve_info(IServer, DataDir) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Info} ->
      cuter_pp:exec_status(ExStatus),
      cuter_pp:exec_info(Info),
      case cuter_analyzer:get_result(ExStatus) of
        internal_error -> cuter_error;
        _ ->
          RawInfo = #{
            dir => DataDir,
            mappings => cuter_analyzer:get_mapping(Info),
            traces => cuter_analyzer:get_traces(Info),
            int => cuter_analyzer:get_int_process(Info),
            tags => cuter_analyzer:get_tags(Info),
            stored_mods => cuter_analyzer:get_stored_modules(Info)
          },
          cuter_analyzer:process_raw_execution_info(RawInfo)
      end;
    {error, Why} ->
      R = {internal_error, iserver, node(), Why},
      cuter_pp:exec_status(R),
      cuter_error
  end.

-spec wait_for_execution(pid()) -> {ok, cuter_iserver:execution_status(), orddict:orddict()} | {error, any()}.
wait_for_execution(IServer) ->
  receive
    {IServer, ExStatus, Info} ->
      ok = wait_for_iserver(IServer),
      {ok, ExStatus, Info};
    {'EXIT', IServer, Why} ->
      {error, Why}
  end.

-spec wait_for_iserver(pid()) -> ok | not_ok.
wait_for_iserver(IServer) ->
receive {'EXIT', IServer, normal} -> ok
after 10000 -> not_ok
end.

