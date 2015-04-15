%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/3, run/4, run/5, run/6, run_once/4, run_once/5]).

-include("include/cuter_macros.hrl").

-type scheduling() :: bfs | maxcover.
-type configuration() :: #{mod => module(),
                           func => atom(),
                           dataDir => file:filename_all(),
                           depth => pos_integer(),
                           no => integer(),
                           scheduler => pid(),
                           scheduler_mod => module(),
                           stored_mods => cuter_analyzer:stored_modules(),
                           tags_added_no => integer()}.


-spec run_once(module(), atom(), [any()], pos_integer()) -> ok.
run_once(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run_once(M, F, As, Depth, CWD).

-spec run_once(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run_once(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir, default_scheduling()),
  run_seed_execution(Conf, As, false).

-spec run(module(), atom(), [any()]) -> ok.
run(M, F, As) ->
  run(M, F, As, ?DEFAULT_DEPTH).

-spec run(module(), atom(), [any()], pos_integer()) -> ok.
run(M, F, As, Depth) ->
  {ok, CWD} = file:get_cwd(),
  run(M, F, As, Depth, CWD).

-spec run(module(), atom(), [any()], pos_integer(), file:filename_all()) -> ok.
run(M, F, As, Depth, BaseDir) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir, default_scheduling()),
  run_seed_execution(Conf, As, true).

-spec run(module(), atom(), [any()], pos_integer(), file:filename_all(), scheduling()) -> ok.
run(M, F, As, Depth, BaseDir, Sched) ->
  Conf = initialize_app(M, F, As, Depth, BaseDir, Sched),
  run_seed_execution(Conf, As, true).

-spec run_seed_execution(configuration(), [any()], boolean()) -> ok.
run_seed_execution(Conf, As, Loop) ->
  case concolic_execute(Conf, As) of
    cuter_error ->
      stop(Conf);
    Info ->
      M = maps:get(scheduler_mod, Conf),
      ok = M:seed_execution(maps:get(scheduler, Conf), Info),
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
  M = maps:get(scheduler_mod, Conf),
  case M:request_input(maps:get(scheduler, Conf)) of
    empty -> stop(Conf);
    {Ref, As, StoredMods, TagsN} ->
      No = maps:get(no, Conf) + 1,
      Conf_n = Conf#{no := No, stored_mods := StoredMods, tags_added_no := TagsN},
      case concolic_execute(Conf_n, As) of
        cuter_error ->
          stop(Conf);
        Info ->
          ok = M:store_execution(maps:get(scheduler, Conf), Ref, Info),
          loop(Conf_n)
      end
  end.

-spec initialize_app(module(), atom(), [any()], pos_integer(), file:filename_all(), scheduling()) -> configuration().
initialize_app(M, F, As, Depth, BaseDir, Sched) ->
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% Disable error_logger
  cuter_pp:mfa(M, F, length(As)),
  SchedMod = get_scheduler_module(Sched),
  SchedPid = SchedMod:start(?PYTHON_CALL, Depth),
  #{mod => M,
    func => F,
    no => 1,
    depth => Depth,
    dataDir => cuter_lib:get_tmp_dir(BaseDir),
    scheduler => SchedPid,
    scheduler_mod => SchedMod,
    stored_mods => orddict:new(),
    tags_added_no => 0}.

-spec stop(configuration()) -> ok.
stop(Conf) ->
  M = maps:get(scheduler_mod, Conf),
  M:stop(maps:get(scheduler, Conf)),
  cuter_lib:clear_and_delete_dir(maps:get(dataDir, Conf)).

-spec default_scheduling() -> scheduling().
default_scheduling() -> maxcover.

-spec get_scheduler_module(scheduling()) -> module().
get_scheduler_module(bfs) -> cuter_bfs_scheduler;
get_scheduler_module(maxcover) -> cuter_scheduler_maxcover.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

-spec concolic_execute(configuration(), [any()]) -> cuter_analyzer:info() | cuter_error.
concolic_execute(Conf, Input) ->
  cuter_pp:input(Input),
  BaseDir = maps:get(dataDir, Conf),
  DataDir = cuter_lib:get_data_dir(BaseDir, maps:get(no, Conf)),
  TraceDir = cuter_lib:get_trace_dir(DataDir),  % Directory to store process traces
  M = maps:get(mod, Conf),
  F = maps:get(func, Conf),
  Depth = maps:get(depth, Conf),
  StoredMods = maps:get(stored_mods, Conf),
  TagsN = maps:get(tags_added_no, Conf),
  IServer = cuter_iserver:start(M, F, Input, TraceDir, Depth, StoredMods, TagsN),
  retrieve_info(IServer, DataDir).

-spec retrieve_info(pid(), file:filename_all()) -> cuter_analyzer:info() | cuter_error.
retrieve_info(IServer, DataDir) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Info} ->
      cuter_pp:exec_status(ExStatus),
      cuter_pp:exec_info(Info),
      case cuter_analyzer:get_result(ExStatus) of
        internal_error -> cuter_error;
        ExResult ->
          RawInfo = #{
            result => ExResult,
            dir => DataDir,
            mappings => cuter_analyzer:get_mapping(Info),
            traces => cuter_analyzer:get_traces(Info),
            int => cuter_analyzer:get_int_process(Info),
            tags => cuter_analyzer:get_tags(Info),
            stored_mods => cuter_analyzer:get_stored_modules(Info),
            tags_added_no => cuter_analyzer:get_no_of_tags_added(Info)
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

