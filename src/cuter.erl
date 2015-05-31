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

-define(DEFAULT_DEPTH, 25).

%% The configuration of the tool.
-record(conf, {
  codeServer    :: pid(),
  mod           :: mod(),
  func          :: atom(),
  dataDir       :: file:filename(),
  depth         :: depth(),
  scheduler     :: pid()
}).
-type configuration() :: #conf{}.

%% Runtime Options
-define(FULLY_VERBOSE_EXEC_INFO, fully_verbose_execution_info).
-define(ENABLE_PMATCH, enable_pmatch).
-define(POLLERS_NO, number_of_pollers).

-type default_option() :: ?ENABLE_PMATCH
                        | {?POLLERS_NO, 1}
                        .

-type option() :: default_option()
                | {basedir, file:filename()}
                | {?POLLERS_NO, pos_integer()}
                | ?FULLY_VERBOSE_EXEC_INFO
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
  start(Conf, number_of_pollers(Options)).

%% ----------------------------------------------------------------------------
%% Manage the concolic executions
%% ----------------------------------------------------------------------------

-spec start(configuration(), 1) -> erroneous_inputs().
start(Conf, N) ->
  CodeServer = Conf#conf.codeServer,
  Scheduler = Conf#conf.scheduler,
  M = Conf#conf.mod,
  F = Conf#conf.func,
  Dir = Conf#conf.dataDir,
  Depth = Conf#conf.depth,
  Pollers = [cuter_poller:start(CodeServer, Scheduler, M, F, Dir, Depth) || _ <- lists:seq(1, N)],
  wait_for_pollers(Conf, Pollers).

-spec wait_for_pollers(configuration(), [pid()]) -> erroneous_inputs().
wait_for_pollers(Conf, []) ->
  stop(Conf);
wait_for_pollers(Conf, Pollers) ->
  receive
    {'EXIT', Who, normal} ->
      wait_for_pollers(Conf, Pollers -- [Who]);
    {'EXIT', Who, Why} ->
      io:format("Poller ~p exited with ~p~n", [Who, Why]),
      wait_for_pollers(Conf, Pollers -- [Who])
  end.

-spec stop(configuration()) -> erroneous_inputs().
stop(Conf) ->
  Erroneous = cuter_scheduler_maxcover:stop(Conf#conf.scheduler),
  CodeLogs = cuter_codeserver:get_logs(Conf#conf.codeServer),
  cuter_codeserver:stop(Conf#conf.codeServer),
  cuter_pp:code_logs(CodeLogs),
  cuter_pp:errors_found(Erroneous),
  cuter_pp:stop(),
  cuter_lib:clear_and_delete_dir(Conf#conf.dataDir),
  Erroneous.

%% ----------------------------------------------------------------------------
%% Initializations
%% ----------------------------------------------------------------------------

-spec initialize_app(mod(), atom(), input(), depth(), [option()]) -> configuration().
initialize_app(M, F, As, Depth, Options) ->
  BaseDir = set_basedir(Options),
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% disable error_logger
  WithPmatch = lists:member(?ENABLE_PMATCH, Options),
  CodeServer = cuter_codeserver:start(self(), WithPmatch),
  SchedPid = cuter_scheduler_maxcover:start(?PYTHON_CALL, Depth, As, CodeServer),
  ok = cuter_pp:start(reporting_level(Options)),
  cuter_pp:mfa({M, F, length(As)}),
  #conf{ codeServer = CodeServer
       , mod = M
       , func = F
       , depth = Depth
       , dataDir = cuter_lib:get_tmp_dir(BaseDir)
       , scheduler = SchedPid}.

%% Set app parameters.
-spec default_options() -> [default_option(), ...].
default_options() ->
  [?ENABLE_PMATCH, {?POLLERS_NO, 1}].

-spec set_basedir([option()]) -> file:filename().
set_basedir([]) -> {ok, CWD} = file:get_cwd(), CWD;
set_basedir([{basedir, BaseDir}|_]) -> BaseDir;
set_basedir([_|Rest]) -> set_basedir(Rest).

-spec number_of_pollers([option()]) -> pos_integer().
number_of_pollers([{?POLLERS_NO, N}|_Rest]) -> N;
number_of_pollers([_|Rest]) -> number_of_pollers(Rest).

-spec reporting_level([option()]) -> cuter_pp:pp_level().
reporting_level(Options) ->
  Default = cuter_pp:default_reporting_level(),
  case lists:member(?FULLY_VERBOSE_EXEC_INFO, Options) of
    false -> Default;
    true  -> cuter_pp:fully_verbose_exec_info(Default)
  end.

