%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_pp).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).
-export([start/0, stop/0]).
%% Report information about the concolic executions.
-export([mfa/1, input/2, error_retrieving_spec/2, execution_status/2,
         execution_info/2, path_vertex/2, flush/1, errors_found/1,
         form_has_unsupported_type/1, invalid_ast_with_pmatch/2, code_logs/1]).
%% Report information about solving.
-export([solving_failed_unsat/0, solving_failed_timeout/0, solving_failed_unknown/0]).
%% Report callgraph related info.
-export([callgraph_calculation_failed/1, loading_visited_module/1]).
%% Parsing of options.
-export([loaded_whitelist/2, error_loading_whitelist/2]).
%% Statistics about the solver.
-export([solved_models/2]).
%% Report coverage.
-export([coverage_title/0, branch_coverage/3, branch_coverage_nocomp/3, condition_coverage/3,
         condition_coverage_nocomp/3, condition_outcome_coverage/3, condition_outcome_coverage_nocomp/3]).
%% Format errors.
-export([abstract_code_missing/1, cover_compiled_module/1, non_existing_module/1,
         compilation_errors/2, non_existing_mfa/1]).
%% Debug Verbose prints.
-export([parsed_spec/1]).
%% Used in unit tests.
-export([pp_argument/1]).
%% Metrics.
-export([pp_metrics_title/0, pp_distribution_metric/2]).

-export([ reversible_operations/1
        %% Execution info reporting level
        , default_reporting_level/0, minimal_exec_info/1, fully_verbose_exec_info/1
        , verbose_exec_info/1
        %% Verbose File/Folder Deletion
        , delete_file/2
        %% Verbose solving
%%        , sat/0
%%        , not_sat/0
        , model_start/0
        , model_end/0
        , received_var/1
        , received_val/1
        , port_closed/0
        , undecoded_msg/2
        , fsm_started/1
        , send_cmd/3
        , debug_unexpected_solver_message/1
        %% Verbose merging
        , file_finished/1
        , set_goal/2
        , consume_msg/1
        , goal_already_achieved/1
        , search_goal_in_file/1
        , change_to_file/1
        , open_file/2
        , achieve_goal/2
        , open_pending_file/1
        %% Pre-run checks
        , module_non_existing/1
        , mfa_non_existing/3
       ]).

-include("include/cuter_macros.hrl").
-include("include/cuter_types.hrl").

-export_type([pp_level/0]).

-define(PRETTY_PRINTER, pretty_printer).

-type from() :: {pid(), reference()}.

%% The data of each concolic execution.
-record(info, {
  executionInfo = dict:new() :: cuter_iserver:logs(),
  executionStatus            :: cuter_iserver:execution_status() | 'undefined',
  input                      :: cuter:input(),
  pathVertex = []            :: cuter_analyzer:path_vertex()
}).
-type execution_data() :: #info{}.

%% The state of the server.
-record(st, {
  super   :: pid(),
  pplevel :: pp_level(),
  nl      :: boolean(),
  mfa     :: mfa() | 'undefined',
  info    :: dict:dict(cuter_scheduler:handle(), execution_data())
}).
-type state() :: #st{}.

%% Types for calls, casts, & replies.

-type coverage_call() :: branch_coverage | branch_coverage_nocomp
                       | condition_coverage | condition_coverage_nocomp
                       | condition_outcome_coverage | condition_outcome_coverage_nocomp.

-type call() :: {module_non_existing, atom()}
              | {mfa_non_existing, mfa()}
              | {mfa, mfa()}
              | {invalid_ast_with_pmatch, module(), any()}
              | {error_retrieving_spec, mfa(), any()}
              | {form_has_unsupported_type, any()}
              | {input, cuter_scheduler:handle(), cuter:input()}
              | {execution_status, cuter_scheduler:handle(), cuter_iserver:execution_status()}
              | {execution_info, cuter_scheduler:handle(), cuter_iserver:logs()}
              | {path_vertex, cuter_scheduler:handle(), cuter_analyzer:path_vertex()}
              | {flush, cuter_scheduler:handle()}
              | solving_failed_unsat
              | solving_failed_timeout
              | solving_failed_unknown
              | {errors_found, cuter:erroneous_inputs()}
              | {code_logs, cuter_codeserver:logs(), cuter_mock:whitelist(), boolean()}
              | {callgraph_calculation_failed, string()}
              | {loading_visited_module, cuter:mod()}
              | {solved_models, non_neg_integer(), non_neg_integer()}
              | coverage_title
              | {coverage_call(), non_neg_integer(), non_neg_integer(), float()}
              | {parsed_spec, cuter_types:erl_spec()}
              .

%% Reporting levels.
-define(MINIMAL, 0).
-define(VERBOSE, 1).
-define(FULLY_VERBOSE, 2).
-type level() :: ?MINIMAL | ?VERBOSE | ?FULLY_VERBOSE.

-record(pp_level, {
  execInfo = ?MINIMAL :: level()
}).
-type pp_level() :: #pp_level{}.

%% ----------------------------------------------------------------------------
%% Manipulate the reporting levels
%% ----------------------------------------------------------------------------

-spec default_reporting_level() -> pp_level().
default_reporting_level() -> #pp_level{}.

-spec minimal_exec_info(pp_level()) -> pp_level().
minimal_exec_info(PpLevel) ->
  PpLevel#pp_level{execInfo = ?MINIMAL}.

-spec verbose_exec_info(pp_level()) -> pp_level().
verbose_exec_info(PpLevel) ->
  PpLevel#pp_level{execInfo = ?VERBOSE}.

-spec fully_verbose_exec_info(pp_level()) -> pp_level().
fully_verbose_exec_info(PpLevel) ->
  PpLevel#pp_level{execInfo = ?FULLY_VERBOSE}.

%% ============================================================================
%% External API
%% ============================================================================

%% Starts the server.
-spec start() -> ok.
start() ->
  case gen_server:start({local, ?PRETTY_PRINTER}, ?MODULE, [self()], []) of
    {ok, _Pid} -> ok;
    {error, Reason} -> exit({failed_to_start_ppserver, Reason})
  end.

%% Stops the server.
-spec stop() -> ok.
stop() ->
  gen_server:cast(?PRETTY_PRINTER, {stop, self()}).

%% The MFA that will be tested.
-spec mfa(mfa()) -> ok.
mfa(MFA) ->
  gen_server:call(?PRETTY_PRINTER, {mfa, MFA}).

%% The input that will be tested.
-spec input(cuter_scheduler:handle(), cuter:input()) -> ok.
input(Ref, As) ->
  gen_server:call(?PRETTY_PRINTER, {input, Ref, As}).

%% Generated invalid Core Erlang AST with pmatch.
-spec invalid_ast_with_pmatch(module(), any()) -> ok.
invalid_ast_with_pmatch(Mod, Generated) ->
  gen_server:call(?PRETTY_PRINTER, {invalid_ast_with_pmatch, Mod, Generated}).

%% Error while retrieving the spec for the MFA.
-spec error_retrieving_spec(mfa(), any()) -> ok.
error_retrieving_spec(MFA, Error) ->
  gen_server:call(?PRETTY_PRINTER, {error_retrieving_spec, MFA, Error}).

%% Encountered unsupported type when parsing a form.
-spec form_has_unsupported_type(any()) -> ok.
form_has_unsupported_type(Info) ->
  gen_server:call(?PRETTY_PRINTER, {form_has_unsupported_type, Info}).

%% The result status of the given concolic execution.
-spec execution_status(cuter_scheduler:handle(), cuter_iserver:execution_status()) -> ok.
execution_status(Ref, Status) ->
  gen_server:call(?PRETTY_PRINTER, {execution_status, Ref, Status}).

%% The InterpreterServer's logs of the given concolic execution.
-spec execution_info(cuter_scheduler:handle(), cuter_iserver:logs()) -> ok.
execution_info(Ref, Logs) ->
  gen_server:call(?PRETTY_PRINTER, {execution_info, Ref, Logs}).

%% The path vertex of the given concolic execution.
-spec path_vertex(cuter_scheduler:handle(), cuter_analyzer:path_vertex()) -> ok.
path_vertex(Ref, Vertex) ->
  gen_server:call(?PRETTY_PRINTER, {path_vertex, Ref, Vertex}).

%% Display the information of the given concolic execution.
-spec flush(cuter_scheduler:handle()) -> ok.
flush(Ref) ->
  gen_server:call(?PRETTY_PRINTER, {flush, Ref}).

%% Print a character to show that solving was unsatisfiable.
-spec solving_failed_unsat() -> ok.
solving_failed_unsat() ->
  gen_server:call(?PRETTY_PRINTER, solving_failed_unsat).

%% Print a character to show that solving timed out.
-spec solving_failed_timeout() -> ok.
solving_failed_timeout() ->
  gen_server:call(?PRETTY_PRINTER, solving_failed_timeout).

%% Print a character to show that solving was unknown.
-spec solving_failed_unknown() -> ok.
solving_failed_unknown() ->
  gen_server:call(?PRETTY_PRINTER, solving_failed_unknown).

%% Print the erroneous input that were found.
-spec errors_found(cuter:erroneous_inputs()) -> ok.
errors_found(Errors) ->
  gen_server:call(?PRETTY_PRINTER, {errors_found, Errors}).

%% Prints the logs of a CodeServer.
-spec code_logs(cuter_codeserver:logs()) -> ok.
code_logs(Logs) ->
  gen_server:call(?PRETTY_PRINTER, {code_logs, Logs}).

%% Print the parsed spec of the MFA.
-spec parsed_spec(cuter_types:erl_spec()) -> ok.
parsed_spec(Spec) ->
  gen_server:call(?PRETTY_PRINTER, {parsed_spec, Spec}).

%% ----------------------------------------------------------------------------
%% Whitelisted MFAs.
%% ----------------------------------------------------------------------------

-spec loaded_whitelist(file:name(), cuter_mock:whitelist()) -> ok.
loaded_whitelist(File, Whitelist) ->
  {ok, VerbosityLevel} = cuter_config:fetch(?VERBOSITY_LEVEL),
  case VerbosityLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "Loaded whitelisted MFAs from ~p.~n", [File]);
    ?VERBOSE ->
      io:format("Loaded whitelisted MFAs from ~p.~n", [File]);
    ?FULLY_VERBOSE ->
      MFAs = cuter_mock:get_whitelisted_mfas(Whitelist),
      io:format("Loaded the following whitelisted MFAs from ~p.~n  ~p~n", [File, MFAs])
  end.

-spec error_loading_whitelist(file:name(), any()) -> ok.
error_loading_whitelist(File, Error) ->
  {ok, VerbosityLevel} = cuter_config:fetch(?VERBOSITY_LEVEL),
  case VerbosityLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "Error ~p occured when loading whitelisted MFAs from ~p.~n", 
                [Error, File]);
    _ ->
      io:format("Error ~p occured when loading whitelisted MFAs from ~p.~n", [Error, File])
  end.

%% ----------------------------------------------------------------------------
%% Failed pre-run checks.
%% ----------------------------------------------------------------------------

-spec module_non_existing(atom()) -> ok.
module_non_existing(M) ->
  gen_server:call(?PRETTY_PRINTER, {module_non_existing, M}).

-spec mfa_non_existing(atom(), atom(), arity()) -> ok.
mfa_non_existing(M, F, Arity) ->
  gen_server:call(?PRETTY_PRINTER, {mfa_non_existing, {M, F, Arity}}).

%% ----------------------------------------------------------------------------
%% Statistics about the solver.
%% ----------------------------------------------------------------------------

-spec solved_models(non_neg_integer(), non_neg_integer()) -> ok.
solved_models(Solved, NotSolved) ->
  gen_server:call(?PRETTY_PRINTER, {solved_models, Solved, NotSolved}).

%% ----------------------------------------------------------------------------
%% Report coverage.
%% ----------------------------------------------------------------------------

-spec coverage_title() -> ok.
coverage_title() ->
  gen_server:call(?PRETTY_PRINTER, coverage_title).

-spec branch_coverage(non_neg_integer(), non_neg_integer(), float()) -> ok.
branch_coverage(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {branch_coverage, Visited, All, Coverage}).

-spec branch_coverage_nocomp(non_neg_integer(), non_neg_integer(), float()) -> ok.
branch_coverage_nocomp(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {branch_coverage_nocomp, Visited, All, Coverage}).

-spec condition_coverage(non_neg_integer(), non_neg_integer(), float()) -> ok.
condition_coverage(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {condition_coverage, Visited, All, Coverage}).

-spec condition_coverage_nocomp(non_neg_integer(), non_neg_integer(), float()) -> ok.
condition_coverage_nocomp(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {condition_coverage_nocomp, Visited, All, Coverage}).

-spec condition_outcome_coverage(non_neg_integer(), non_neg_integer(), float()) -> ok.
condition_outcome_coverage(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {condition_outcome_coverage, Visited, All, Coverage}).

-spec condition_outcome_coverage_nocomp(non_neg_integer(), non_neg_integer(), float()) -> ok.
condition_outcome_coverage_nocomp(Visited, All, Coverage) ->
  gen_server:call(?PRETTY_PRINTER, {condition_outcome_coverage_nocomp, Visited, All, Coverage}).

%% ----------------------------------------------------------------------------
%% Callgraph related info.
%% ----------------------------------------------------------------------------

-spec callgraph_calculation_failed(string()) -> ok.
callgraph_calculation_failed(Reason) ->
  gen_server:call(?PRETTY_PRINTER, {callgraph_calculation_failed, Reason}).

-spec loading_visited_module(cuter:mod()) -> ok.
loading_visited_module(M) ->
  gen_server:call(?PRETTY_PRINTER, {loading_visited_module, M}).

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([pid(), ...]) -> {ok, state()}.
init([Super]) ->
  %% process_flag(trap_exit, true),
  link(Super),
  {ok, PpLevel} = cuter_config:fetch(?VERBOSITY_LEVEL),
  {ok, #st{ info = dict:new()
          , nl = false
          , pplevel = PpLevel
          , super = Super}}.

%% gen_server callback : terminate/2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call(call(), from(), state()) -> {reply, ok, state()}.


%% Report coverage.

handle_call(coverage_title, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL -> io:format(standard_error, "~nCoverage Metrics ...~n", []);
    _ -> io:format("~nCoverage Statistics ...~n")
  end,
  {reply, ok, State};

handle_call({branch_coverage, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p clauses (~.2f %).~n", [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p clauses (~.2f %).~n", [Visited, All, Coverage])
  end,
  {reply, ok, State};

handle_call({branch_coverage_nocomp, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p clauses [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p clauses [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage])
  end,
  {reply, ok, State};

handle_call({condition_coverage, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p conditions (~.2f %).~n", [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p conditions (~.2f %).~n", [Visited, All, Coverage])
  end,
  {reply, ok, State};

handle_call({condition_coverage_nocomp, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p conditions [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p conditions [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage])
  end,
  {reply, ok, State};

handle_call({condition_outcome_coverage, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p condition outcomes (~.2f %).~n", [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p condition outcomes (~.2f %).~n", [Visited, All, Coverage])
  end,
  {reply, ok, State};

handle_call({condition_outcome_coverage_nocomp, Visited, All, Coverage}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL ->
      io:format(standard_error, "  - ~p of ~p condition outcomes [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage]);
    _ ->
      io:format("  - ~p of ~p condition outcomes [w/o compiler generated clauses] (~.2f %).~n",
        [Visited, All, Coverage])
  end,
  {reply, ok, State};

%% Statistics about the solver.

handle_call({solved_models, Solved, NotSolved}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL -> ok;
    _ ->
      io:format("~nSolver Statistics ...~n"),
      io:format("  - Solved models   : ~w~n", [Solved]),
      io:format("  - Unsolved models : ~w~n", [NotSolved])
  end,
  {reply, ok, State};

%% Callgraph related info.

handle_call({callgraph_calculation_failed, Reason}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL -> io:format(standard_error, "ERROR~nFailed to calculate the callgraph because~n  ~p~n", [Reason]);
    _ -> io:format("ERROR~nFailed to calculate the callgraph because~n  ~p~n", [Reason])
  end,
  {reply, ok, State};
handle_call({loading_visited_module, M}, _From, State=#st{pplevel = PpLevel}) ->
  case PpLevel#pp_level.execInfo of
    ?MINIMAL -> ok;
    _ -> io:format("Loading module ~p.~n", [M])
  end,
  {reply, ok, State};

%% Pre-run check: Non-existing module.
handle_call({module_non_existing, M}, _From, State) ->
  io:format("\033[01;31mModule ~p does not exist.\033[00m~n", [M]),
  {reply, ok, State};
%% Pre-run check: Non-existing mfa.
handle_call({mfa_non_existing, {M, F, Arity}}, _From, State) ->
  io:format("\033[01;31mMFA ~p:~p/~w does not exist or is not exported.\033[00m~n", [M, F, Arity]),
  {reply, ok, State};
%% The MFA to be tested.
handle_call({mfa, {M, F, Ar}}, _From, State) ->
  io:format("\033[0;34mTesting ~p:~p/~p ...\033[00m~n", [M, F, Ar]),
  {reply, ok, State#st{mfa = {M, F, Ar}}};
%% Generated invalid Core Erlang AST with pmatch.
handle_call({invalid_ast_with_pmatch, Mod, Generated}, _From, State=#st{pplevel = PpLevel}) ->
  case get(invalid_ast) of
    undefined ->
      put(invalid_ast, [Mod]),
      invalid_ast(Mod, Generated, PpLevel#pp_level.execInfo),
      {reply, ok, State};
    Mods ->
      case lists:member(Mod, Mods) of
        true -> {reply, ok, State};
        false ->
          invalid_ast(Mod, Generated, PpLevel#pp_level.execInfo),
          put(invalid_ast, [Mod|Mods]),
          {reply, ok, State}
      end
  end;
%% Error retrieving the spec of the MFA.
handle_call({error_retrieving_spec, MFA, Error}, _From, State=#st{pplevel = PpLevel}) ->
  case get(spec_error) of
    yes -> {reply, ok, State};
    undefined ->
      put(spec_error, yes),
      spec_error(MFA, Error, PpLevel#pp_level.execInfo),
      {reply, ok, State}
  end;
%% Encountered an unsupported type
handle_call({form_has_unsupported_type, Info}, _From, State=#st{pplevel = PpLevel}) ->
  case get(type_error) of
    yes -> {reply, ok, State};
    undefined ->
      put(type_error, yes),
      unsupported_type_error(Info, PpLevel#pp_level.execInfo),
      {reply, ok, State}
  end;
%% A new input to be tested.
handle_call({input, Ref, Input}, _From, State=#st{info = Info}) ->
  Info1 = dict:store(Ref, #info{input = Input}, Info),
  {reply, ok, State#st{info = Info1}};
%% The status of the given concolic execution.
handle_call({execution_status, Ref, Status}, _From, State=#st{info = Info}) ->
  Info1 = dict:update(Ref, fun(Data) -> Data#info{executionStatus = Status} end, Info),
  {reply, ok, State#st{info = Info1}};
%% The information of the given concolic execution.
handle_call({execution_info, Ref, ExecInfo}, _From, State=#st{info = Info}) ->
  Info1 = dict:update(Ref, fun(Data) -> Data#info{executionInfo = ExecInfo} end, Info),
  {reply, ok, State#st{info = Info1}};
%% The path vertex of the given concolic execution.
handle_call({path_vertex, Ref, Vertex}, _From, State=#st{info = Info}) ->
  Info1 = dict:update(Ref, fun(Data) -> Data#info{pathVertex = Vertex} end, Info),
  {reply, ok, State#st{info = Info1}};
%% Display the information of the given concolic execution.
handle_call({flush, Ref}, _From, State=#st{pplevel = PpLevel, info = Info, nl = Nl, mfa = MFA}) ->
  pp_execution_info(dict:fetch(Ref, Info), MFA, Nl, PpLevel#pp_level.execInfo),
  Info1 = dict:erase(Ref, Info),
  case PpLevel#pp_level.execInfo of
    ?MINIMAL -> {reply, ok, State#st{info = Info1, nl = true}};
    _ -> {reply, ok, State#st{info = Info1, nl = false}}
  end;
%% Print a character to show that solving failed to product an output.
handle_call(SlvFail, _From, State=#st{pplevel = PpLevel}) when SlvFail =:= solving_failed_unsat;
                                                               SlvFail =:= solving_failed_timeout;
                                                               SlvFail =:= solving_failed_unknown ->
  pp_solving_failure(SlvFail, PpLevel#pp_level.execInfo),
  {reply, ok, State#st{nl = true}};
%% Report the errors that were found.
handle_call({errors_found, Errors}, _From, State=#st{pplevel = PpLevel}) ->
  pp_erroneous_inputs(Errors, PpLevel#pp_level.execInfo),
  {reply, ok, State#st{nl = false}};
%% Prints the logs of a CodeServer.
handle_call({code_logs, CodeLogs}, _From, State=#st{pplevel = PpLevel}) ->
  {ok, Whitelist} = cuter_config:fetch(?WHITELISTED_MFAS),
  pp_code_logs(CodeLogs, Whitelist, PpLevel#pp_level.execInfo),
  {reply, ok, State#st{nl = false}};
%% Prints the parsed spec of the mfa.
handle_call({parsed_spec, Spec}, _From, State=#st{mfa = Mfa, pplevel = PpLevel}) ->
  case get(parsed_spec) of
    yes -> {reply, ok, State};
    undefined ->
      put(parsed_spec, yes),
      pp_parsed_spec(Mfa, Spec, PpLevel#pp_level.execInfo),
      {reply, ok, State}
  end.

%% gen_server callback : handle_cast/2
-spec handle_cast({stop, pid()}, state()) -> {stop, normal, state()} | {noreply, state()}.
%% Stops the server.
handle_cast({stop, FromWho}, State=#st{super = Super}) ->
  case FromWho =:= Super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end.

%% gen_server callback : handle_info/2
%% Msg when a monitored process exited normally
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_What, State) ->
  {noreply, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec spec_error(mfa(), any(), level()) -> ok.
spec_error(MFA, {unsupported_type, Name}, Level) ->
  IODevice =
    case Level of
      ?MINIMAL -> standard_error;
      _ -> standard_io
    end,
  io:format(IODevice, "~nWARNING: The spec of ~p uses the unsupported type ~p!~n"
    ++ "  It has been generalized to any().~n~n", [MFA, Name]);
spec_error(_MFA, _Error, ?MINIMAL) ->
  ok;
spec_error(MFA, not_found, _Level) ->
  io:format("~nWARNING: Could not find the spec of ~p!~n~n", [MFA]);
spec_error(MFA, {could_not_access, {Mod, Type, Arity}}, _Level) ->
  io:format("~nWARNING: Could not access the definition of type ~p:~p/~w~n"
    ++ "  for the spec of ~p!~n"
    ++ "  It has been generalized to any().~n~n", [Mod, Type, Arity, MFA]);
spec_error(MFA, has_remote_types, _Level) ->
  io:format("~nWARNING: The spec of ~p has a remote type and is not supported!~n~n", [MFA]);
spec_error(MFA, {recursive_type, {Mod, Type, Arity}}, _Level) ->
  io:format("~nWARNING: The spec of ~p contains the recursive type ~p:~p/~w and is not supported!~n"
    ++ "  It has been generalized to any().~n~n", [MFA, Mod, Type, Arity]);
spec_error(MFA, {recursive_type, TVar}, _Level) ->
  io:format("~nWARNING: The spec of ~p contains the recursive type ~p and is not supported!~n"
    ++ "  It has been generalized to any().~n~n", [MFA, TVar]);
spec_error(MFA, Error, _Level) ->
  io:format("~nWARNING: Error while retrieving the spec of ~p!~n  Error: ~p~n~n", [MFA, Error]).

-spec invalid_ast(module(), any(), level()) -> ok.
invalid_ast(_Mod, _Generated, ?MINIMAL) ->
  ok;
invalid_ast(Mod, Generated, _Level) ->
  io:format("~nWARNING: Generated invalid Core Erlang AST for module ~p!~n  ~p~n~n", [Mod, Generated]).

-spec unsupported_type_error(any(), level()) -> ok.
unsupported_type_error(_Form, ?MINIMAL) ->
  ok;
unsupported_type_error(Form, _Level) ->
  io:format("~nWARNING: Encountered an unsupported type while parsing the types in Core Erlang forms!~n"),
  io:format("  Form: ~p~n~n", [Form]).

-spec pp_nl(boolean(), boolean()) -> ok.
pp_nl(true, true) -> io:format(standard_error, "~n", []);
pp_nl(true, false) -> io:format("~n");
pp_nl(false, _) -> ok.

-spec pp_solving_failure(solving_failed_unsat | solving_failed_timeout | solving_failed_unknown, level()) -> ok.
pp_solving_failure(solving_failed_unsat, ?MINIMAL) -> io:format(standard_error, "x", []);
pp_solving_failure(solving_failed_unsat, _) -> io:format("x");
pp_solving_failure(solving_failed_timeout, ?MINIMAL) -> io:format(standard_error, "t", []);
pp_solving_failure(solving_failed_timeout, _) -> io:format("t");
pp_solving_failure(solving_failed_unknown, ?MINIMAL) -> io:format(standard_error, "u", []);
pp_solving_failure(solving_failed_unknown, _) -> io:format("u").

-spec pp_execution_info(execution_data(), mfa(), boolean(), level()) -> ok.
pp_execution_info(Data, MFA, Nl, ?MINIMAL) ->
  ExecutionStatus = Data#info.executionStatus,
  case cuter_analyzer:is_runtime_error(ExecutionStatus) of
    false -> pp_execution_status_minimal(Data#info.executionStatus);
    true  -> pp_nl(Nl, true), pp_input_minimal(Data#info.input, MFA)
  end;
pp_execution_info(Data, MFA, Nl, ?VERBOSE) ->
  pp_nl(Nl, false),
  pp_input_verbose(Data#info.input, MFA),
  pp_execution_status_verbose(Data#info.executionStatus),
  pp_nl(true, false);
pp_execution_info(Data, MFA, Nl, ?FULLY_VERBOSE) ->
  pp_nl(Nl, false),
  pp_input_fully_verbose(Data#info.input, MFA),
  pp_execution_status_fully_verbose(Data#info.executionStatus),
  pp_execution_logs_fully_verbose(Data#info.executionInfo),
  pp_path_vertex_fully_verbose(Data#info.pathVertex).

%% ----------------------------------------------------------------------------
%% Report the execution status
%% ----------------------------------------------------------------------------

-spec pp_execution_status_minimal(cuter_iserver:execution_status()) -> ok.
pp_execution_status_minimal({success, _Result}) ->
  io:format(standard_error, ".", []);
pp_execution_status_minimal({runtime_error, _RuntimeError}) ->
  io:format(standard_error, "E", []);
pp_execution_status_minimal({internal_error, _InternalError}) ->
  io:format(standard_error, "I", []).

-spec pp_execution_status_verbose(cuter_iserver:execution_status()) -> ok.

pp_execution_status_verbose({success, _Result}) ->
  io:format("\033[01;32mok\033[00m");
pp_execution_status_verbose({runtime_error, _RuntimeError}) ->
  io:format("\033[01;31mRUNTIME ERROR\033[00m");
pp_execution_status_verbose({internal_error, _InternalError}) ->
  io:format("\033[01;31mINTERNAL ERROR\033[00m").

-spec pp_execution_status_fully_verbose(cuter_iserver:execution_status()) -> ok.
pp_execution_status_fully_verbose({success, Result}) ->
  io:format("OK~n"),
  io:format("  CONCRETE: ~p~n", [cuter_eval:get_concrete(Result)]),
  io:format("  SYMBOLIC: ~p~n", [cuter_eval:get_symbolic(Result)]);
pp_execution_status_fully_verbose({runtime_error, {Node, Pid, Result}}) ->
  io:format("RUNTIME ERROR~n"),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  PID: ~p~n", [Pid]),
  io:format("  CONCRETE: ~p~n", [cuter_eval:get_concrete(Result)]),
  io:format("  SYMBOLIC: ~p~n", [cuter_eval:get_symbolic(Result)]);
pp_execution_status_fully_verbose({internal_error, {Type, Node, Why}}) ->
  io:format("INTERNAL ERROR~n"),
  io:format("  TYPE: ~p~n", [Type]),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  REASON: ~p~n", [Why]).

%% ----------------------------------------------------------------------------
%% Report the input
%% ----------------------------------------------------------------------------

-spec pp_input_minimal(cuter:input(), mfa()) -> ok.
pp_input_minimal([], {M, F, _}) ->
  io:format(standard_error, "\033[01;31m~p:~p()\033[00m~n", [M, F]);
pp_input_minimal(Input, {M, F, _}) ->
  FLs = preprocess_input(Input),
  S = pp_arguments(Input, FLs),
  As = pp_assignments(FLs),
  io:format(standard_error, "\033[01;31m~s~p:~p(\033[00m", [As, M, F]),
  io:format(standard_error, "\033[01;31m~s)\033[00m~n", [S]).

-spec pp_input_verbose(cuter:input(), mfa()) -> ok.
pp_input_verbose([], {M, F, _}) ->
  io:format("~p:~p()~n", [M, F]);
pp_input_verbose(Input, {M, F, _}) ->
  FLs = preprocess_input(Input),
  S = pp_arguments(Input, FLs),
  io:format("~s~p:~p(~s) ... ", [pp_assignments(FLs), M, F, S]).

-spec pp_input_fully_verbose(cuter:input(), mfa()) -> ok.
pp_input_fully_verbose(Input, _MFA) ->
  FLs = preprocess_input(Input),
  divider("="),
  io:format("INPUT~n"),
  io:format("~s~s~n", [pp_assignments(FLs), pp_arguments(Input, FLs)]).

pp_assignments(FLs) ->
  PP_Fn = fun(Fn) -> pp_argument(Fn, proplists:delete(Fn, FLs)) end,
  Ss = [io_lib:format("~s = ~s, ", [[L, $0], PP_Fn(Fn)]) || {Fn, L} <- FLs],
  string:join(Ss, "").

pp_arguments(Args, FLs) ->
  string:join([pp_argument(A, FLs) || A <- Args], ", ").

preprocess_input(I) ->
  Fns = lists:flatten([preprocess_fn(A, false) || A <- I]),
  Ls = [$A + N || N <- lists:seq(0, length(Fns) - 1)],
  lists:zip(Fns, Ls).

preprocess_fn(A, InFn) ->
  case cuter_lib:is_lambda(A) of
    false when is_list(A) ->
      lists:flatten([preprocess_fn(E, InFn) || E <- A]);
    false when is_tuple(A) ->
      lists:flatten([preprocess_fn(E, InFn) || E <- tuple_to_list(A)]);
    false ->
      [];
    true when InFn ->
      [A | preprocess_fn(A, false)];
    true ->
      KVs = cuter_lib:lambda_kvs(A),
      AllKs = lists:flatten([Ks || {Ks, _} <- KVs]),
      lists:flatten([preprocess_fn(E, true) || E <- AllKs])
  end.

-spec pp_argument(term()) -> string().
pp_argument(Arg) ->
  pp_argument(Arg, []).

pp_argument(Arg, FLs) ->
  pp_argument(Arg, FLs, false).

pp_argument(Arg, FLs, IsP) ->
  case cuter_lib:is_lambda(Arg) of
    true ->
      case proplists:lookup(Arg, FLs) of
        none ->
          KVs = cuter_lib:lambda_kvs(Arg),
          Default = cuter_lib:lambda_default(Arg),
          Arity = cuter_lib:lambda_arity(Arg),
          ClauseList = pp_lambda_kvs(KVs, FLs) ++ [pp_lambda_default(Default, Arity)],
          lists:flatten(["fun", string:join(ClauseList, "; "), " end"]);
        {_, L} when IsP ->
          [L];
        {_, L} ->
          [L, $0]
      end;
    false when is_list(Arg) ->
      case io_lib:printable_list(Arg) of
        true ->
          io_lib:format("~p", [Arg]);
        false ->
          "[" ++ string:join([pp_argument(A, FLs) || A <- Arg], ",") ++ "]"
      end;
    false when is_tuple(Arg) ->
      L = tuple_to_list(Arg),
      "{" ++ string:join([pp_argument(A, FLs) || A <- L], ",") ++ "}";
    false ->
      io_lib:format("~p", [cuter_lib:handle_unbound_var(Arg)])
  end.

pp_lambda_kvs(KVs, FLs) ->
  [pp_lambda_kv(KV, FLs) || KV <- KVs].

pp_lambda_kv({Args, Val}, FLs) ->
  SArgs = [pp_argument(A, FLs, true) || A <- Args],
  Ls = lists:flatten([find_inner_vars(A, FLs) || A <- Args]),
  G = case length(Ls) of
        0 ->
          "";
        _ ->
          GLs = [io_lib:format("~s =:= ~s", [[L], [L,  $0]]) || L <- Ls],
	  io_lib:format(" when ~s", [string:join(GLs, ", ")])
      end,
  StrArgs = ["(", string:join(SArgs, ","), ")"],
  lists:flatten([StrArgs, G, " -> ", pp_argument(Val, [])]).

pp_lambda_default(X, Arity) ->
  Args = string:join(["_" || _ <- lists:seq(1, Arity)], ","),
  lists:flatten(["(", Args, ") -> ", pp_argument(X, [])]).

find_inner_vars(A, FLs) ->
  Fn = fun(E) -> E =/= none end,
  lists:filter(Fn, find_inner_vars_h(A, FLs)).

find_inner_vars_h(A, FLs) ->
  case cuter_lib:is_lambda(A) of
    false when is_list(A) ->
      lists:flatten([find_inner_vars_h(E, FLs) || E <- A]);
    false when is_tuple(A) ->
      lists:flatten([find_inner_vars_h(E, FLs) || E <- tuple_to_list(A)]);
    false ->
      [none];
    true ->
      case proplists:lookup(A, FLs) of
        none ->
          [none];
        {_, L} ->
          [L]
      end
  end.

%% ----------------------------------------------------------------------------
%% Report the execution logs
%% ----------------------------------------------------------------------------

-spec pp_execution_logs_fully_verbose(cuter_iserver:logs()) -> ok.
pp_execution_logs_fully_verbose(Logs) ->
  io:format("EXECUTION INFO~n"),
  F = fun(Node, NodeLogs) ->
    io:format("  ~p~n", [Node]),
    pp_node_logs(NodeLogs)
  end,
  cuter_iserver:logs_foreach(F, Logs).

-spec pp_node_logs(cuter_iserver:node_logs()) -> ok.
pp_node_logs(Logs) ->
  %% Int process
  Int = cuter_iserver:int_of_node_logs(Logs),
  io:format("    INT~n"),
  io:format("      ~p~n", [Int]),
  %% Mapping
  Ms = cuter_iserver:mapping_of_node_logs(Logs),
  io:format("    MAPPING~n"),
  F = fun({Sv, Cv}) -> io:format("      ~p <=> ~p~n", [Sv, Cv]) end,
  lists:foreach(F, Ms),
  %% MonitorServer logs
  MonitorLogs = cuter_iserver:monitorLogs_of_node_logs(Logs),
  io:format("    MONITOR LOGS~n"),
  Dir = cuter_monitor:dir_of_logs(MonitorLogs),
  io:format("      DIR~n"),
  io:format("        ~p~n", [Dir]),
  Procs = cuter_monitor:procs_of_logs(MonitorLogs),
  io:format("      PROCS~n"),
  io:format("        ~p~n", [Procs]).

%% ----------------------------------------------------------------------------
%% Report the CodeServer's logs
%% ----------------------------------------------------------------------------

-spec pp_code_logs(cuter_codeserver:logs(), cuter_mock:whitelist(), level()) -> ok.
pp_code_logs(Logs, Whitelist, L) when L =:= ?MINIMAL orelse L =:= ?VERBOSE ->
  case cuter_config:fetch(?SUPPRESS_UNSUPPORTED_MFAS) of
    {ok, true} ->
      ok;
    _ ->
      pp_unsupported_mfas(Logs, Whitelist)
  end;
pp_code_logs(Logs, Whitelist, ?FULLY_VERBOSE) ->
  pp_code_logs_fully_verbose(Logs, Whitelist).

%% Reports the unsupported mfas.
-spec pp_unsupported_mfas(cuter_codeserver:logs(), cuter_mock:whitelist()) -> ok.
pp_unsupported_mfas(Logs, Whitelist) ->
  UnsupportedMfas = cuter_codeserver:unsupportedMfas_of_logs(Logs),
  case filter_non_reportable(UnsupportedMfas, Whitelist) of
    [] -> ok;
    Mfas ->
      {Internal, External} =
        lists:partition(fun cuter_mock:is_internal_without_symbolic_representation/1, Mfas),
      io:format("~n=== BIFs Currently without Symbolic Interpretation ===~n"),
      lists:foreach(fun pp_mfa/1, External),
      pp_internal_unsupported(Internal)
  end.

pp_internal_unsupported([]) ->
  ok;
pp_internal_unsupported(Mfas) ->
  io:format("~n=== Internal BIFs Where Precision Was Lost ===~n"),
  lists:foreach(fun pp_mfa/1, Mfas).

pp_mfa({M, F, A}) ->
  io:format("~p:~p/~w~n", [M, F, A]).

-spec pp_code_logs_fully_verbose(cuter_codeserver:logs(), cuter_mock:whitelist()) -> ok.
pp_code_logs_fully_verbose(Logs, Whitelist) ->
  io:format("~n"),
  divider("-"),
  io:format("CODE LOGS~n"),
  %% Cached modules.
  Cached = cuter_codeserver:cachedMods_of_logs(Logs),
  CMs = cuter_codeserver:modules_of_dumped_cache(Cached),
  io:format("    CACHED MODS~n"),
  io:format("      ~p~n", [lists:sort(CMs)]),
  %% Loaded modules.
  LMs = cuter_codeserver:loadedMods_of_logs(Logs),
  io:format("    LOADED MODS~n"),
  io:format("      ~p~n", [lists:sort(LMs)]),
  %% Number of added tags.
  N = cuter_codeserver:tagsAddedNo_of_logs(Logs),
  io:format("    NUMBER OF ADDED TAGS~n"),
  io:format("      ~p~n", [N]),
  %% Visited tags.
  Tags = cuter_codeserver:visitedTags_of_logs(Logs),
  io:format("    VISITED TAGS~n"),
  io:format("      ~p~n", [gb_sets:to_list(Tags)]),
  %% Unsupported MFAs.
  MFAs = cuter_codeserver:unsupportedMfas_of_logs(Logs),
  FilteredMFAs = filter_non_reportable(MFAs, Whitelist),
  io:format("    UNSUPPORTED MFAS~n"),
  io:format("      ~p~n", [lists:sort(FilteredMFAs)]).

%% Filters out the non-reportable mfas.
%% These include
%%   1) whitelisted mfas
%%   2) mfas that there is no meaning in evaluating them symbolically.
filter_non_reportable(Mfas, Whitelist) ->
  lists:filter(
    fun(Mfa) ->
        not cuter_mock:no_symbolic_evalution(Mfa) andalso
        not cuter_mock:is_whitelisted(Mfa, Whitelist)
      end,
    Mfas
  ).

%% ----------------------------------------------------------------------------
%% Report the path vertex
%% ----------------------------------------------------------------------------

-spec pp_path_vertex_fully_verbose(cuter_analyzer:path_vertex()) -> ok.
pp_path_vertex_fully_verbose(Vertex) ->
  io:format("PATH VERTEX~n"),
  io:format("  ~p (~w)~n", [Vertex, length(Vertex)]).

%% ----------------------------------------------------------------------------
%% Report the erroneous inputs
%% ----------------------------------------------------------------------------

has_errors(Errors) ->
  [] =/= lists:flatten([Es || {_Mfa, Es} <- Errors]).

-spec pp_erroneous_inputs(cuter:erroneous_inputs(), level()) -> ok.
pp_erroneous_inputs(Errors, Level) ->
  pp_nl(true, Level =:= ?MINIMAL),
  case has_errors(Errors) of
    false ->
      io:format("No Runtime Errors Occured~n");
    true ->
      io:format("=== Inputs That Lead to Runtime Errors ==="),
      pp_erroneous_inputs_h(Errors, 1)
  end.

pp_erroneous_inputs_h([], _N) ->
  io:format("~n");
pp_erroneous_inputs_h([{Mfa, Errors}|Rest], N) ->
  N1 = pp_erroneous_inputs_mfa(Errors, Mfa, N),
  pp_erroneous_inputs_h(Rest, N1).

pp_erroneous_inputs_mfa([], _MFA, N) ->
  N;
pp_erroneous_inputs_mfa([I|Is], {M, F, _}=MFA, N) ->
  FLs = preprocess_input(I),
  S = pp_arguments(I, FLs),
  As = pp_assignments(FLs),
  io:format("~n#~w \033[00;31m~s~p:~p(~s)\033[00m", [N, As, M, F, S]),
  pp_erroneous_inputs_mfa(Is, MFA, N + 1).

%% ----------------------------------------------------------------------------
%% Format errors.
%% ----------------------------------------------------------------------------

-spec abstract_code_missing(cuter:mod()) -> string().
abstract_code_missing(M) ->
  lists:flatten(io_lib:format("Could not retrieve the Abstract Code for module ~p.", [M])).

-spec cover_compiled_module(cuter:mod()) -> string().
cover_compiled_module(M) ->
  lists:flatten(io_lib:format("Module ~p is cover compiled.", [M])).

-spec non_existing_module(cuter:mod()) -> string().
non_existing_module(M) ->
  lists:flatten(io_lib:format("Could not find module ~p.", [M])).

-spec compilation_errors(cuter:mod(), any()) -> string().
compilation_errors(M, Errors) ->
  lists:flatten(io_lib:format("Compilation of module ~p failed with ~p.", [M, Errors])).

-spec non_existing_mfa(mfa()) -> string().
non_existing_mfa(Mfa) ->
  lists:flatten(io_lib:format("Could not find module mfa ~p.", [Mfa])).

%% ----------------------------------------------------------------------------
%% FIXME Remaining to be updated
%% ----------------------------------------------------------------------------

-spec reversible_operations(integer()) -> ok.
reversible_operations(RvsCnt) ->
  io:format("REVERSIBLE OPERATIONS~n"),
  io:format("  ~w~n", [RvsCnt]).

divider(Divider) ->
  lists:foreach(fun(_) -> io:format(Divider) end, lists:seq(1,50)),
  io:format("~n").

%%
%% Verbose File/Folder Deletion
%%

-spec delete_file(file:name(), boolean()) -> ok.
-ifdef(VERBOSE_FILE_DELETION).
delete_file(F, true) ->
  io:format("[DELETE] ~p (OK)~n", [F]);
delete_file(F, false) ->
  io:format("[DELETE] ~p (LEAVE INTACT)~n", [F]).
-else.
delete_file(_F, _Intact) -> ok.
-endif.

%% ----------------------------------------------------------------------------
%% Solver FSM
%% ----------------------------------------------------------------------------

-spec model_start() -> ok.
model_start() ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV] (generating_model) Beginning of the model~n");
    _ ->
      ok
  end.

-spec model_end() -> ok.
model_end() ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV] (expecting_var) End of the model~n");
    _ ->
      ok
  end.

-spec received_var(cuter_symbolic:symbolic()) -> ok.
received_var(Var) ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV] (expecting_var) Var: ~p~n", [Var]);
    _ ->
      ok
  end.

-spec received_val(any()) -> ok.
received_val(Val) ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV] (expecting_value) Val: ~p~n", [Val]);
    _ ->
      ok
  end.

-spec port_closed() -> ok.
port_closed() ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV] (finished) Port Closed~n");
    _ ->
      ok
  end.

-spec undecoded_msg(binary(), cuter_solver:state()) -> ok.
undecoded_msg(Msg, State) ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[SLV INFO] (~p) ~p~n", [State, Msg]);
    _ ->
      ok
  end.

-spec fsm_started(port()) -> ok.
fsm_started(Port) ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[FSM] (idle) Started (Port ~p)~n", [Port]);
    _ ->
      ok
  end.

-type cmd() :: {file:name(), integer()} | cuter_symbolic:mapping() | binary().	% FIXME
-spec send_cmd(cuter_solver:state(), cmd(), string()) -> ok.
send_cmd(State, _, Descr) ->
  case cuter_config:fetch(?DEBUG_SOLVER_FSM) of
    {ok, true} ->
      io:format("[FSM] (~p) ~p~n", [State, Descr]);
    _ ->
      ok
  end.

-spec debug_unexpected_solver_message(any()) -> ok.
debug_unexpected_solver_message(Info) ->
  io:format("[SOLVER ERROR] ~p~n", [Info]).

%%
%% Verbose Merging
%%

-spec file_finished(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
file_finished(File) ->
  io:format("[MERGE] Fully parsed ~p~n", [File]).
-else.
file_finished(_File) -> ok.
-endif.

-spec set_goal(cuter_merger:goal(), string()) -> ok.
-ifdef(VERBOSE_MERGING).
set_goal(Goal, Tp) ->
  io:format("[MERGE] (~p) Set Goal ~p~n", [Tp, Goal]).
-else.
set_goal(_Goal, _Tp) -> ok.
-endif.

%% XXX: Strengthen the argument type
-spec consume_msg(term()) -> ok.
-ifdef(VERBOSE_MERGING).
consume_msg(Msg) ->
  io:format("[MERGE] (MSG CONSUME) ~p~n", [Msg]).
-else.
consume_msg(_Msg) -> ok.
-endif.

-spec goal_already_achieved(cuter_merger:goal()) -> ok.
-ifdef(VERBOSE_MERGING).
goal_already_achieved(Goal) ->
  io:format("[MERGE] Already achieved ~p~n", [Goal]).
-else.
goal_already_achieved(_Goal) -> ok.
-endif.

-spec search_goal_in_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
search_goal_in_file(File) ->
  io:format("[MERGE] Will look in ~p~n", [File]).
-else.
search_goal_in_file(_File) -> ok.
-endif.

-spec change_to_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
change_to_file(File) ->
  io:format("[MERGE] Changing to ~p~n", [File]).
-else.
change_to_file(_File) -> ok.
-endif.

-spec open_file(file:name(), [file:name()]) -> ok.
-ifdef(VERBOSE_MERGING).
open_file(File, Pending) ->
  io:format("[MERGE] Opening ~p~n", [File]),
  io:format("  Pending files~n"),
  io:format("    ~p~n", [Pending]).
-else.
open_file(_File, _Pending) -> ok.
-endif.

-spec achieve_goal(cuter_merger:goal(), cuter_merger:goal()) -> ok.
-ifdef(VERBOSE_MERGING).
achieve_goal(Goal, NextGoal) ->
  io:format("[MERGE] Achieved ~p~n", [Goal]),
  io:format("  Changing to ~p~n", [NextGoal]).
-else.
achieve_goal(_Goal, _NextGoal) -> ok.
-endif.

-spec open_pending_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
open_pending_file(File) ->
  io:format("[MERGE] Opening from pending ~p~n", [File]).
-else.
open_pending_file(_File) -> ok.
-endif.

%% ----------------------------------------------------------------------------
%% Various debug prints.
%% ----------------------------------------------------------------------------

pp_parsed_spec(_Mfa, _Spec, ?MINIMAL) ->
  ok;
pp_parsed_spec({M, F, A}, {Spec, TypeDeps}, _Level) ->
  io:format("\033[01;34m~p:~p/~w\033[00m~n", [M, F, A]),
  Cs = [pp_type(C, fun io_lib:format/2) || C <- Spec],
  io:format("    Spec:~n"),
  io:format("        \033[01;33m~s\033[00m~n", [string:join(Cs, " | ")]),
  io:format("    Types:~n"),
  pp_typedeps(TypeDeps),
  io:nl().

-spec pp_typedeps(cuter_types:erl_type_deps()) -> ok.
pp_typedeps(TypeDeps) ->
  lists:foreach(fun pp_typedep/1, TypeDeps).

pp_typedep({Name, Type}) ->
  io:format("        \033[01;36m~s\033[00m =>~n", [Name]),
  io:format("            \033[01;33m~s\033[00m~n", [pp_type(Type, fun io_lib:format/2)]).

pp_type(Type, FmtFn) ->
  StrFmtFn = fun io_lib:format/2,
  case cuter_types:get_kind(Type) of
    ?any_tag ->
      FmtFn("any()", []);
    ?atom_tag ->
      FmtFn("atom()", []);
    ?atom_lit_tag ->
      FmtFn("~w", [cuter_types:atom_of_t_atom_lit(Type)]);
    ?float_tag ->
      FmtFn("float()", []);
    ?integer_tag ->
      FmtFn("integer()", []);
    ?integer_lit_tag ->
      FmtFn("~w", [cuter_types:integer_of_t_integer_lit(Type)]);
    ?list_tag ->
      FmtFn("[~s]", [pp_type(cuter_types:elements_type_of_t_list(Type), StrFmtFn)]);
    ?nonempty_list_tag ->
      FmtFn("[~s, ...]", [pp_type(cuter_types:elements_type_of_t_nonempty_list(Type), StrFmtFn)]);
    ?nil_tag ->
      FmtFn("[]", []);
    ?bitstring_tag ->
      {M, N} = cuter_types:segment_size_of_bitstring(Type),
      FmtFn("<<_:~w, _:_*~w>>", [M, N]);
    ?tuple_tag ->
      %% TODO Distinguish between tuple() and {}.
      case cuter_types:elements_types_of_t_tuple(Type) of
        [] ->
          FmtFn("tuple()", []);
        Ts ->
          Ts1 = [pp_type(T, StrFmtFn) || T <- Ts],
          FmtFn("{~s}", [string:join(Ts1, ", ")])
      end;
    ?union_tag ->
      Ts1 = [pp_type(T, StrFmtFn) || T <- cuter_types:elements_types_of_t_union(Type)],
      FmtFn("~s", [string:join(Ts1, " | ")]);
    ?range_tag ->
      {Lower, Upper} = cuter_types:bounds_of_t_range(Type),
      FmtFn("~s..~s", [pp_range_bound(Lower), pp_range_bound(Upper)]);
    ?function_tag ->
      FmtFn("~s", [pp_typesig(Type, StrFmtFn)]);
    ?userdef_tag ->
      FmtFn("~s", [cuter_types:name_of_t_userdef(Type)])
  end.

pp_range_bound(Limit) ->
  case cuter_types:get_kind(Limit) of
    ?integer_lit_tag ->
      integer_to_list(cuter_types:integer_of_t_integer_lit(Limit));
    Kind when Kind =:= ?pos_inf orelse Kind =:= ?neg_inf ->
      ""
  end.

pp_typesig(Fun, FmtFn) ->
  StrFmtFn = fun io_lib:format/2,
  Ret = pp_type(cuter_types:ret_of_t_function(Fun), StrFmtFn),
  case cuter_types:is_generic_function(Fun) of
    true ->
      FmtFn("fun((...) -> ~s)", [Ret]);
    false ->
      Params = [pp_type(P, StrFmtFn) || P <- cuter_types:params_of_t_function_det(Fun)],
      FmtFn("fun((~s) -> ~s)", [string:join(Params, ", "), Ret])
  end.

%% ----------------------------------------------------------------------------
%% Report collected metrics.
%% ----------------------------------------------------------------------------

-spec pp_metrics_title() -> ok.
pp_metrics_title() ->
  io:format("~n=== Collected Metrics ===~n~n").

-spec pp_distribution_metric(cuter_metrics:name(), cuter_metrics:distribution_list()) -> ok.
pp_distribution_metric(Name, Distribution) ->
  io:format("[~s]~n", [Name]),
  pp_distribution_values(Distribution).

pp_distribution_values(D) ->
  lists:foreach(fun pp_distribution_value/1, D).

pp_distribution_value({V, N}) ->
  io:format("- ~s: ~w~n", [V, N]).
