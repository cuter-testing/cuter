%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/2, run/3, run/4, run/5, run_from_file/2]).

-export_type([func/0, mod/0, input/0, erroneous_inputs/0, depth/0]).

-include("include/cuter_macros.hrl").
-include("include/cuter_metrics.hrl").

-type mod()   :: module().
-type func()  :: atom().
-type input() :: [any()].
-type depth() :: pos_integer().
-type erroneous_inputs() :: [{mfa(), [input()]}].
-type seed()    :: {module(), atom(), input(), depth()}.
-type options() :: proplists:proplist().  %% [debug_options() | runtime_options()].

-define(ONE, 1).
-define(TWO, 2).
-define(DEFAULT_DEPTH, 25).
-define(DEFAULT_STRATEGY, cuter_bfs_strategy).

%% The state of the tool.
-record(st, {
  codeServer  :: pid(),
  scheduler   :: pid(),
  seeds = []  :: [seed()],
  errors = [] :: erroneous_inputs()
}).
-type state() :: #st{}.

%% ----------------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------------

-spec run(mod(), atom(), input()) -> erroneous_inputs().
run(M, F, As) ->
  run(M, F, As, ?DEFAULT_DEPTH).

-spec run(mod(), atom(), input(), depth()) -> erroneous_inputs().
run(M, F, As, Depth) ->
  run(M, F, As, Depth, []).

-spec run(mod(), atom(), input(), depth(), options()) -> erroneous_inputs().
run(M, F, As, Depth, Options) ->
  Seeds = [{M, F, As, Depth}],
  run(Seeds, Options).

-spec run([seed()], options()) -> erroneous_inputs().
%% Runs CutEr on multiple units.
run(Seeds, Options) ->
  State = initialize_app(Options),
  StateWithSeeds = add_seeds(State, Seeds),
  Mfas = [{M, F, length(As)} || {M, F, As, _} <- Seeds],
  case pre_run_checks(Mfas) of
    error ->
      stop(StateWithSeeds);
    ok ->
      case preprocess(StateWithSeeds, Mfas) of
        ok ->
          start(StateWithSeeds);
        error ->
          stop(StateWithSeeds)
      end
  end.

add_seeds(State, Seeds) ->
  State#st{ seeds = Seeds }.

-spec run_from_file(file:name(), options()) -> erroneous_inputs().
%% Loads the seeds from a file and runs CutEr on all of them.
%% The terms in the file need to be in form:
%%   {M :: module(), F :: atom(), SeedInput :: [input()], Depth :: pos_integer()}.
run_from_file(File, Options) ->
  case file:consult(File) of
    {ok, Seeds} ->
      run(Seeds, Options);
    Error ->
      throw({error_loading_file, Error})
  end.

%% ----------------------------------------------------------------------------
%% Pre-run checks
%% ----------------------------------------------------------------------------

-spec pre_run_checks([mfa()]) -> ok | error.
pre_run_checks([]) ->
  ok;
pre_run_checks([{M, F, A}|Rest]) ->
  case code:which(M) of
    non_existing ->
      cuter_pp:module_non_existing(M),
      error;
    _ ->
      Exports = M:module_info(exports),
      case lists:member({F, A}, Exports) of
        true ->
          pre_run_checks(Rest);
        false ->
          cuter_pp:mfa_non_existing(M, F, A),
          error
      end
  end.

%% ----------------------------------------------------------------------------
%% Preprocessing
%% ----------------------------------------------------------------------------

-spec preprocess(state(), [mfa()]) -> ok | error.
preprocess(State, Mfas) ->
  case cuter_config:fetch(?CALCULATE_COVERAGE) of
    {ok, true} ->
      cuter_codeserver:calculate_callgraph(State#st.codeServer, Mfas);
    _ ->
      ok
  end.

%% ----------------------------------------------------------------------------
%% Manage the concolic executions
%% ----------------------------------------------------------------------------

-spec start(state()) -> erroneous_inputs().
start(State) ->
  start(State#st.seeds, State).

-spec start([seed()], state()) -> erroneous_inputs().
start([], State) ->
  stop_and_report(State);
start([{M, F, As, Depth}|Seeds], State) ->
  CodeServer = State#st.codeServer,
  Scheduler = State#st.scheduler,
  Errors = start_one(M, F, As, Depth, CodeServer, Scheduler),
  NewErrors = [{{M, F, length(As)}, Errors}|State#st.errors],
  io:nl(),
  start(Seeds, State#st{errors = NewErrors}).

start_one(M, F, As, Depth, CodeServer, Scheduler) ->
  cuter_pp:mfa({M, F, length(As)}),
  ok = cuter_scheduler:add_seed_input(Scheduler, As),
  ok = cuter_scheduler:set_depth(Scheduler, Depth),
  {ok, N_Pollers} = cuter_config:fetch(?NUM_POLLERS),
  {ok, Dir} = cuter_config:fetch(?WORKING_DIR),
  Pollers = [cuter_poller:start(CodeServer, Scheduler, M, F, Dir, Depth) || _ <- lists:seq(1, N_Pollers)],
  {ok, N_Solvers} = cuter_config:fetch(?NUM_SOLVERS),
  Solvers = [cuter_solver:start(Scheduler) || _ <- lists:seq(1, N_Solvers)],
  ok = wait_for_processes(Pollers, fun cuter_poller:send_stop_message/1),
  LiveSolvers = lists:filter(fun is_process_alive/1, Solvers),
  lists:foreach(fun cuter_solver:send_stop_message/1, LiveSolvers),
  ok = wait_for_processes(LiveSolvers, fun cuter_solver:send_stop_message/1),
  ErroneousInputs = cuter_scheduler:get_erroneous_inputs(Scheduler),
  ok = cuter_scheduler:clear_erroneous_inputs(Scheduler),
  ErroneousInputs.

-spec wait_for_processes([cuter_poller:poller()], fun((cuter_poller:poller()) -> ok)) -> ok.
wait_for_processes([], _StopFn) ->
  ok;
wait_for_processes(Procs, StopFn) ->
  receive
    {'EXIT', Who, normal} ->
      wait_for_processes(Procs -- [Who], StopFn);
    {'EXIT', Who, Why} ->
      io:format("Proccess ~p exited with ~p~n", [Who, Why]),
      io:format("Shutting down the execution...~n"),
      Rest = Procs -- [Who],
      lists:foreach(StopFn, Rest),
      wait_for_processes(Rest, StopFn)
  end.

-spec stop_and_report(state()) -> erroneous_inputs().
stop_and_report(State) ->
  %% Report solver statistics.
  SolvedModels = cuter_scheduler:get_solved_models(State#st.scheduler),
  NotSolvedModels = cuter_scheduler:get_not_solved_models(State#st.scheduler),
  cuter_analyzer:solving_stats(SolvedModels, NotSolvedModels),
  %% Report coverage statistics.
  VisitedTags = cuter_scheduler:get_visited_tags(State#st.scheduler),
  case cuter_config:fetch(?CALCULATE_COVERAGE) of
    {ok, true} ->
      cuter_analyzer:calculate_coverage(State#st.codeServer, VisitedTags);
    _ ->
      ok
  end,
  %% Report solver statistics.
  case cuter_config:fetch(?REPORT_METRICS) of
    {ok, true} ->
      cuter_analyzer:report_metrics();
    _ ->
      ok
  end,
  %% Report the erroneous inputs.
  ErroneousInputs = maybe_sort_errors(lists:reverse(State#st.errors)),
  cuter_pp:errors_found(ErroneousInputs),
  %% Report the code logs.
  CodeLogs = cuter_codeserver:get_logs(State#st.codeServer),
  cuter_pp:code_logs(CodeLogs),
  stop(State, ErroneousInputs).

maybe_sort_errors(ErroneousInputs) ->
  case cuter_config:fetch(?SORTED_ERRORS) of
    {ok, true} ->
      [{Mfa, lists:sort(Errors)} || {Mfa, Errors} <- ErroneousInputs];
    _ ->
      ErroneousInputs
  end.

-spec stop(state()) -> erroneous_inputs().
stop(State) ->
  stop(State, []).

-spec stop(state(), erroneous_inputs()) -> erroneous_inputs().
stop(State, ErroneousInputs) ->
  cuter_scheduler:stop(State#st.scheduler),
  cuter_codeserver:stop(State#st.codeServer),
  cuter_pp:stop(),
  cuter_metrics:stop(),
  case cuter_config:fetch(?DEBUG_KEEP_TRACES) of
    {ok, true} ->
      ok;
    _ ->
      {ok, Dir} = cuter_config:fetch(?WORKING_DIR),
      cuter_lib:clear_and_delete_dir(Dir)
  end,
  cuter_config:stop(),
  ErroneousInputs.

%% ----------------------------------------------------------------------------
%% Initializations
%% ----------------------------------------------------------------------------

-spec initialize_app(options()) -> state().
initialize_app(Options) ->
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% disable error_logger
  ok = cuter_config:start(),
  ok = cuter_metrics:start(),
  ok = define_metrics(),
  enable_debug_config(Options),
  enable_runtime_config(Options),
  ok = cuter_pp:start(),
  CodeServer = cuter_codeserver:start(),
  SchedPid = cuter_scheduler:start(?DEFAULT_DEPTH, CodeServer),
  #st{ codeServer = CodeServer, scheduler = SchedPid }.

define_metrics() ->
  define_distribution_metrics().

define_distribution_metrics() ->
  lists:foreach(fun cuter_metrics:define_distribution_metric/1, ?DISTRIBUTION_METRICS).

-spec enable_debug_config(options()) -> ok.
enable_debug_config(Options) ->
  cuter_config:store(?DEBUG_KEEP_TRACES, proplists:get_bool(?DEBUG_KEEP_TRACES, Options)),
  cuter_config:store(?DEBUG_SMT, proplists:get_bool(?DEBUG_SMT, Options)),
  cuter_config:store(?DEBUG_SOLVER_FSM, proplists:get_bool(?DEBUG_SOLVER_FSM, Options)).

-spec enable_runtime_config(options()) -> ok.
enable_runtime_config(Options) ->
  {ok, CWD} = file:get_cwd(),
  cuter_config:store(?WORKING_DIR,
                     cuter_lib:get_tmp_dir(proplists:get_value(?WORKING_DIR, Options, CWD))),
  cuter_config:store(?VERBOSITY_LEVEL, verbosity_level(Options)),
  cuter_config:store(?Z3_TIMEOUT, proplists:get_value(?Z3_TIMEOUT, Options, ?TWO)),
  cuter_config:store(?STRATEGY,
                     proplists:get_value(?STRATEGY, Options, ?DEFAULT_STRATEGY)),
  cuter_config:store(?REPORT_METRICS, proplists:get_bool(?REPORT_METRICS, Options)),
  cuter_config:store(?CALCULATE_COVERAGE, proplists:get_bool(?CALCULATE_COVERAGE, Options)),
  cuter_config:store(?DISABLE_PMATCH, proplists:get_bool(?DISABLE_PMATCH, Options)),
  cuter_config:store(?SUPPRESS_UNSUPPORTED_MFAS, 
                     proplists:get_bool(?SUPPRESS_UNSUPPORTED_MFAS, Options)),
  cuter_config:store(?DISABLE_TYPE_NORMALIZATION, 
                     proplists:get_bool(?DISABLE_TYPE_NORMALIZATION, Options)),
  cuter_config:store(?SORTED_ERRORS, proplists:get_bool(?SORTED_ERRORS, Options)),
  cuter_config:store(?WHITELISTED_MFAS, whitelisted_mfas(Options)),
  cuter_config:store(?NUM_SOLVERS, proplists:get_value(?NUM_SOLVERS, Options, ?ONE)),
  cuter_config:store(?NUM_POLLERS, proplists:get_value(?NUM_POLLERS, Options, ?ONE)),
  cuter_config:store(?ANNOTATIONS, annotations(Options)).

verbosity_level(Options) ->
  Default = cuter_pp:default_reporting_level(),
  case proplists:get_bool(?FULLY_VERBOSE_EXECUTION, Options) of
    true  -> cuter_pp:fully_verbose_exec_info(Default);
    false ->
      case proplists:get_bool(?VERBOSE_EXECUTION, Options) of
        true  -> cuter_pp:verbose_exec_info(Default);
        false -> Default
      end
  end.

whitelisted_mfas(Options) ->
  case proplists:get_value(?WHITELISTED_MFA_PATH, Options) of
    undefined ->
      cuter_mock:empty_whitelist();
    File ->
      case file:consult(File) of
        {ok, LoadedData} ->
          Whitelist = cuter_mock:parse_whitelist(LoadedData),
          cuter_pp:loaded_whitelist(File, Whitelist),
          Whitelist;
        Error ->
          cuter_pp:error_loading_whitelist(File, Error),
          cuter_mock:empty_whitelist()
      end
  end.

annotations(Options) -> annotations(Options, []).

annotations([], Acc) -> Acc;
annotations([{?ANNOTATIONS, M, F}|T], Acc) -> annotations(T, [{M, F}|Acc]);
annotations([_|T], Acc) -> annotations(T, Acc).
