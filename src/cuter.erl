%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/3, run/4, run/5]).

-export_type([mod/0, input/0, erroneous_inputs/0, depth/0]).

-include("include/cuter_macros.hrl").

-type mod() :: atom().  % a subtype of module()
-type input() :: [any()].
-type depth() :: pos_integer().
-type erroneous_inputs() :: [input()].

-define(ONE, 1).
-define(DEFAULT_DEPTH, 25).

%% The configuration of the tool.
-record(conf, {
  codeServer          :: pid(),
  mod                 :: mod(),
  func                :: atom(),
  dataDir             :: file:filename(),
  depth               :: depth(),
  scheduler           :: pid(),
  calculateCoverage   :: boolean(),
  sortErrors          :: boolean(),
  whitelist           :: cuter_mock:whitelist(),
  suppressUnsupported :: boolean()
}).
-type configuration() :: #conf{}.

%% Runtime Options
-define(FULLY_VERBOSE_EXEC_INFO, fully_verbose_execution_info).
-define(VERBOSE_EXEC_INFO, verbose_execution_info).
-define(DISABLE_PMATCH, disable_pmatch).
-define(POLLERS_NO, number_of_pollers).
-define(SOLVERS_NO, number_of_solvers).
-define(WHITELISTED_MFAS, whitelist).
-define(CALCULATE_COVERAGE, coverage).
-define(SORTED_ERRORS, sorted_errors).
-define(SUPPRESS_UNSUPPORTED_MFAS, suppress_unsupported).

-type default_option() :: {?POLLERS_NO, ?ONE}
                        .

-type option() :: default_option()
                | {basedir, file:filename()}
                | {?POLLERS_NO, pos_integer()}
                | {?SOLVERS_NO, pos_integer()}
                | ?FULLY_VERBOSE_EXEC_INFO
                | ?VERBOSE_EXEC_INFO
                | ?DISABLE_PMATCH
                | {?WHITELISTED_MFAS, file:filename()}
                | ?CALCULATE_COVERAGE
                | ?SORTED_ERRORS
                | ?SUPPRESS_UNSUPPORTED_MFAS
                .

%% ----------------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------------

-spec run(mod(), atom(), input()) -> erroneous_inputs().
run(M, F, As) ->
  run(M, F, As, ?DEFAULT_DEPTH).

-spec run(mod(), atom(), input(), depth()) -> erroneous_inputs().
run(M, F, As, Depth) ->
  run(M, F, As, Depth, default_options()).

-spec run(mod(), atom(), input(), depth(), [option()]) -> erroneous_inputs().
run(M, F, As, Depth, Options) ->
  Conf = initialize_app(M, F, As, Depth, Options),
  Mfa = {M, F, length(As)},
  case pre_run_checks(Mfa) of
    error -> stop(Conf);
    ok ->
      case preprocess(Conf, Mfa) of 
        false -> stop(Conf);
        true  -> start(Conf, number_of_pollers(Options), number_of_solvers(Options))
      end
  end.

%% ----------------------------------------------------------------------------
%% Pre-run checks
%% ----------------------------------------------------------------------------

pre_run_checks({M, F, A}) ->
  case code:which(M) of
    non_existing ->
      cuter_pp:module_non_existing(M),
      error;
    _ ->
      Exports = M:module_info(exports),
      case lists:member({F, A}, Exports) of
        true -> ok;
        false ->
          cuter_pp:mfa_non_existing(M, F, A),
          error
      end
  end.

%% ----------------------------------------------------------------------------
%% Preprocessing
%% ----------------------------------------------------------------------------

-spec preprocess(configuration(), mfa()) -> boolean().
preprocess(Conf, Mfa) ->
  preprocess_coverage(Conf, Mfa, Conf#conf.calculateCoverage).

-spec preprocess_coverage(configuration(), mfa(), boolean()) -> boolean().
preprocess_coverage(_Conf, _Mfa, false) -> true;
preprocess_coverage(Conf, Mfa, true) ->
  ok == cuter_codeserver:calculate_callgraph(Conf#conf.codeServer, Mfa).

%% ----------------------------------------------------------------------------
%% Manage the concolic executions
%% ----------------------------------------------------------------------------

-spec start(configuration(), pos_integer(), pos_integer()) -> erroneous_inputs().
start(Conf, N_Pollers, N_Solvers) ->
  CodeServer = Conf#conf.codeServer,
  Scheduler = Conf#conf.scheduler,
  M = Conf#conf.mod,
  F = Conf#conf.func,
  Dir = Conf#conf.dataDir,
  Depth = Conf#conf.depth,
  Pollers = [cuter_poller:start(CodeServer, Scheduler, M, F, Dir, Depth) || _ <- lists:seq(1, N_Pollers)],
  Solvers = [cuter_solver:start(Scheduler) || _ <- lists:seq(1, N_Solvers)],
  ok = wait_for_processes(Pollers, fun cuter_poller:send_stop_message/1),
  LiveSolvers = lists:filter(fun is_process_alive/1, Solvers),
  lists:foreach(fun cuter_solver:send_stop_message/1, LiveSolvers),
  ok = wait_for_processes(LiveSolvers, fun cuter_solver:send_stop_message/1),
  stop_and_report(Conf).

-spec wait_for_processes([pid()], fun((pid()) -> ok)) -> ok.
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

-spec stop_and_report(configuration()) -> erroneous_inputs().
stop_and_report(Conf) ->
  %% Report solver statistics.
  SolvedModels = cuter_scheduler_maxcover:get_solved_models(Conf#conf.scheduler),
  NotSolvedModels = cuter_scheduler_maxcover:get_not_solved_models(Conf#conf.scheduler),
  cuter_analyzer:solving_stats(SolvedModels, NotSolvedModels),
  %% Report coverage statistics.
  VisitedTags = cuter_scheduler_maxcover:get_visited_tags(Conf#conf.scheduler),
  cuter_analyzer:calculate_coverage(Conf#conf.calculateCoverage, Conf#conf.codeServer, VisitedTags),
  %% Report the erroneous inputs.
  ErroneousInputs = cuter_scheduler_maxcover:get_erroneous_inputs(Conf#conf.scheduler),
  ErroneousInputs1 = maybe_sort_errors(Conf#conf.sortErrors, ErroneousInputs),
  cuter_pp:errors_found(ErroneousInputs1),
  %% Report the code logs.
  CodeLogs = cuter_codeserver:get_logs(Conf#conf.codeServer),
  cuter_pp:code_logs(CodeLogs, Conf#conf.whitelist, Conf#conf.suppressUnsupported),
  stop(Conf, ErroneousInputs1).

maybe_sort_errors(false, ErroneousInputs) ->
  ErroneousInputs;
maybe_sort_errors(true, ErroneousInputs) ->
  lists:sort(ErroneousInputs).

-spec stop(configuration()) -> erroneous_inputs().
stop(Conf) ->
  stop(Conf, []).

-spec stop(configuration(), erroneous_inputs()) -> erroneous_inputs().
stop(Conf, ErroneousInputs) ->
  cuter_scheduler_maxcover:stop(Conf#conf.scheduler),
  cuter_codeserver:stop(Conf#conf.codeServer),
  cuter_pp:stop(),
  cuter_lib:clear_and_delete_dir(Conf#conf.dataDir),
  ErroneousInputs.

%% ----------------------------------------------------------------------------
%% Initializations
%% ----------------------------------------------------------------------------

-spec initialize_app(mod(), atom(), input(), depth(), [option()]) -> configuration().
initialize_app(M, F, As, Depth, Options) ->
  BaseDir = set_basedir(Options),
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% disable error_logger
  ok = cuter_pp:start(reporting_level(Options)),
  WithPmatch = with_pmatch(Options),
  Whitelist = get_whitelist(Options),
  CodeServer = cuter_codeserver:start(self(), WithPmatch, Whitelist),
  SchedPid = cuter_scheduler_maxcover:start(?PYTHON_CALL, Depth, As, CodeServer),
  cuter_pp:mfa({M, F, length(As)}),
  #conf{ codeServer = CodeServer
       , mod = M
       , func = F
       , depth = Depth
       , dataDir = cuter_lib:get_tmp_dir(BaseDir)
       , scheduler = SchedPid
       , calculateCoverage = calculate_coverage(Options)
       , sortErrors = sort_errors(Options)
       , whitelist = Whitelist
       , suppressUnsupported = suppress_unsupported_mfas(Options)}.

%% ----------------------------------------------------------------------------
%% Set app parameters
%% ----------------------------------------------------------------------------

-spec default_options() -> [default_option(), ...].
default_options() ->
  [{?POLLERS_NO, 1}].

-spec set_basedir([option()]) -> file:filename().
set_basedir([]) -> {ok, CWD} = file:get_cwd(), CWD;
set_basedir([{basedir, BaseDir}|_]) -> BaseDir;
set_basedir([_|Rest]) -> set_basedir(Rest).

-spec number_of_pollers([option()]) -> pos_integer().
number_of_pollers([]) -> ?ONE;
number_of_pollers([{?POLLERS_NO, N}|_Rest]) -> N;
number_of_pollers([_|Rest]) -> number_of_pollers(Rest).

-spec number_of_solvers([option()]) -> pos_integer().
number_of_solvers([]) -> ?ONE;
number_of_solvers([{?SOLVERS_NO, N}|_Rest]) -> N;
number_of_solvers([_|Rest]) -> number_of_solvers(Rest).

-spec with_pmatch([option()]) -> boolean().
with_pmatch(Options) -> not lists:member(?DISABLE_PMATCH, Options).

-spec get_whitelist([option()]) -> cuter_mock:whitelist().
get_whitelist([]) ->
  cuter_mock:empty_whitelist();
get_whitelist([{?WHITELISTED_MFAS, File}|_]) ->
  case file:consult(File) of
    {ok, LoadedData} ->
      Whitelist = cuter_mock:parse_whitelist(LoadedData),
      cuter_pp:loaded_whitelist(File, Whitelist),
      Whitelist;
    Error ->
      cuter_pp:error_loading_whitelist(File, Error),
      cuter_mock:empty_whitelist()
  end;
get_whitelist([_|Rest]) ->
  get_whitelist(Rest).

-spec reporting_level([option()]) -> cuter_pp:pp_level().
reporting_level(Options) ->
  Default = cuter_pp:default_reporting_level(),
  case lists:member(?FULLY_VERBOSE_EXEC_INFO, Options) of
    true  -> cuter_pp:fully_verbose_exec_info(Default);
    false ->
      case lists:member(?VERBOSE_EXEC_INFO, Options) of
        true  -> cuter_pp:verbose_exec_info(Default);
        false -> Default
      end
  end.

-spec calculate_coverage([option()]) -> boolean().
calculate_coverage(Options) -> lists:member(?CALCULATE_COVERAGE, Options).

-spec sort_errors([option()]) -> boolean().
sort_errors(Options) -> lists:member(?SORTED_ERRORS, Options).

-spec suppress_unsupported_mfas([option()]) -> boolean().
suppress_unsupported_mfas(Options) -> lists:member(?SUPPRESS_UNSUPPORTED_MFAS, Options).
