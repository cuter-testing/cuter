%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/2, run/3, run/4, run/5, run_from_file/2]).

-export_type([mod/0, input/0, erroneous_inputs/0, depth/0]).

-include("include/cuter_macros.hrl").

-type mod() :: atom().  % a subtype of module()
-type input() :: [any()].
-type depth() :: pos_integer().
-type erroneous_inputs() :: [{mfa(), [input()]}].
-type seed() :: {module(), atom(), input(), depth()}.

-define(ONE, 1).
-define(TWO, 2).
-define(DEFAULT_DEPTH, 25).
-define(GUESS, guess).

%% The configuration of the tool.
-record(conf, {
  codeServer          :: pid(),
  dataDir             :: file:filename(),
  scheduler           :: pid(),
  calculateCoverage   :: boolean(),
  sortErrors          :: boolean(),
  whitelist           :: cuter_mock:whitelist(),
  suppressUnsupported :: boolean(),
  seeds = []          :: [seed()],
  nPollers            :: pos_integer(),
  nSolvers            :: pos_integer(),
  errors = []         :: erroneous_inputs()
}).
-type configuration() :: #conf{}.
-type solver() :: z3 | z3py | cvc4 | priority | guess | race | magic.

%% Runtime Options.
-define(FULLY_VERBOSE_EXEC_INFO, fully_verbose_execution_info).
-define(VERBOSE_EXEC_INFO, verbose_execution_info).
-define(DISABLE_PMATCH, disable_pmatch).
-define(POLLERS_NO, number_of_pollers).
-define(SOLVERS_NO, number_of_solvers).
-define(WHITELISTED_MFAS, whitelist).
-define(CALCULATE_COVERAGE, coverage).
-define(SORTED_ERRORS, sorted_errors).
-define(SUPPRESS_UNSUPPORTED_MFAS, suppress_unsupported).
-define(NO_TYPE_NORMALIZATION, no_type_normalization).
-define(Z3_TIMEOUT, z3_timeout).
-define(SOLVER, solver).

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
                | ?NO_TYPE_NORMALIZATION
                | {?Z3_TIMEOUT, pos_integer()}
                | {?SOLVER, solver()}
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
  Seeds = [{M, F, As, Depth}],
  run(Seeds, Options).

-spec run([seed()], [option()]) -> erroneous_inputs().
%% Runs CutEr on multiple units.
run(Seeds, Options) ->
  Conf = initialize_app(Options),
  ConfWithSeeds = add_seeds(Conf, Seeds),
  Mfas = [{M, F, length(As)} || {M, F, As, _} <- Seeds],
  case pre_run_checks(Mfas) of
    error ->
      stop(ConfWithSeeds);
    ok ->
      case preprocess(ConfWithSeeds, Mfas) of
        false -> stop(ConfWithSeeds);
        true  -> start(ConfWithSeeds)
      end
  end.

-spec run_from_file(file:name(), [option()]) -> erroneous_inputs().
%% Loads the seeds from a file and runs CutEr on all of them.
%% The terms in the file needs to be in form:
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

-spec preprocess(configuration(), [mfa()]) -> boolean().
preprocess(Conf, Mfas) ->
  preprocess_coverage(Conf, Mfas, Conf#conf.calculateCoverage).

-spec preprocess_coverage(configuration(), [mfa()], boolean()) -> boolean().
preprocess_coverage(_Conf, _Mfas, false) -> true;
preprocess_coverage(Conf, Mfas, true) ->
  ok =:= cuter_codeserver:calculate_callgraph(Conf#conf.codeServer, Mfas).

%% ----------------------------------------------------------------------------
%% Manage the concolic executions
%% ----------------------------------------------------------------------------

-spec start(configuration()) -> erroneous_inputs().
start(Conf) ->
  start(Conf#conf.seeds, Conf).

-spec start([seed()], configuration()) -> erroneous_inputs().
start([], Conf) ->
  stop_and_report(Conf);
start([{M, F, As, Depth}|Seeds], Conf) ->
  CodeServer = Conf#conf.codeServer,
  Scheduler = Conf#conf.scheduler,
  Dir = Conf#conf.dataDir,
  N_Pollers = Conf#conf.nPollers,
  N_Solvers = Conf#conf.nSolvers,
  Errors = start_one(M, F, As, Depth, CodeServer, Scheduler, Dir, N_Pollers, N_Solvers),
  NewErrors = [{{M, F, length(As)}, Errors}|Conf#conf.errors],
  Conf1 = Conf#conf{errors = NewErrors},
  io:nl(),
  start(Seeds, Conf1).

start_one(M, F, As, Depth, CodeServer, Scheduler, Dir, N_Pollers, N_Solvers) ->
  cuter_pp:mfa({M, F, length(As)}),
  ok = cuter_scheduler_maxcover:add_seed_input(Scheduler, As),
  ok = cuter_scheduler_maxcover:set_depth(Scheduler, Depth),
  Pollers = [cuter_poller:start(CodeServer, Scheduler, M, F, Dir, Depth) || _ <- lists:seq(1, N_Pollers)],
  Solvers = [cuter_solver:start(Scheduler) || _ <- lists:seq(1, N_Solvers)],
  ok = wait_for_processes(Pollers, fun cuter_poller:send_stop_message/1),
  LiveSolvers = lists:filter(fun is_process_alive/1, Solvers),
  lists:foreach(fun cuter_solver:send_stop_message/1, LiveSolvers),
  ok = wait_for_processes(LiveSolvers, fun cuter_solver:send_stop_message/1),
  ErroneousInputs = cuter_scheduler_maxcover:get_erroneous_inputs(Scheduler),
  ok = cuter_scheduler_maxcover:clear_erroneous_inputs(Scheduler),
  ErroneousInputs.

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
  ErroneousInputs = lists:reverse(Conf#conf.errors),
  ErroneousInputs1 = maybe_sort_errors(Conf#conf.sortErrors, ErroneousInputs),
  cuter_pp:errors_found(ErroneousInputs1),
  %% Report the code logs.
  CodeLogs = cuter_codeserver:get_logs(Conf#conf.codeServer),
  cuter_pp:code_logs(CodeLogs, Conf#conf.whitelist, Conf#conf.suppressUnsupported),
  stop(Conf, ErroneousInputs1).

-spec maybe_sort_errors(boolean(), erroneous_inputs()) -> erroneous_inputs().
maybe_sort_errors(false, ErroneousInputs) ->
  ErroneousInputs;
maybe_sort_errors(true, ErroneousInputs) ->
  [{Mfa, lists:sort(Errors)} || {Mfa, Errors} <- ErroneousInputs].

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

-spec initialize_app([option()]) -> configuration().
initialize_app(Options) ->
  BaseDir = set_basedir(Options),
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% disable error_logger
  ok = cuter_pp:start(reporting_level(Options)),
  WithPmatch = with_pmatch(Options),
  SolverBackend = get_solver_backend(Options),
  Whitelist = get_whitelist(Options),
  NormalizeTypes = type_normalization(Options),
  CodeServer = cuter_codeserver:start(self(), WithPmatch, Whitelist, NormalizeTypes),
  SchedPid = cuter_scheduler_maxcover:start(SolverBackend, ?DEFAULT_DEPTH, CodeServer),
  #conf{ calculateCoverage = calculate_coverage(Options)
       , codeServer = CodeServer
       , dataDir = cuter_lib:get_tmp_dir(BaseDir)
       , nPollers = number_of_pollers(Options)
       , nSolvers = number_of_solvers(Options)
       , scheduler = SchedPid
       , sortErrors = sort_errors(Options)
       , suppressUnsupported = suppress_unsupported_mfas(Options)
       , whitelist = Whitelist }.

add_seeds(Conf, Seeds) ->
  Conf#conf{ seeds = Seeds }.

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

-spec get_solver([option()]) -> solver().
get_solver([]) -> ?GUESS;
get_solver([{?SOLVER, S}|_Rest]) -> S;
get_solver([_|Rest]) -> get_solver(Rest).

-spec z3_timeout([option()]) -> pos_integer().
z3_timeout([]) -> ?TWO;
z3_timeout([{?Z3_TIMEOUT, N}|_Rest]) -> N;
z3_timeout([_|Rest]) -> z3_timeout(Rest).

-spec get_solver_backend([option()]) -> string().
get_solver_backend(Options) ->
  S = get_solver(Options),
  T = z3_timeout(Options),
  string:join([?PYTHON_CALL, "--solver", atom_to_list(S), "--timeout", integer_to_list(T)], " ").

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

-spec type_normalization([option()]) -> boolean().
type_normalization(Options) -> not lists:member(?NO_TYPE_NORMALIZATION, Options).
