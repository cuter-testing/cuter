%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------

%%====================================================================
%% Types
%%====================================================================

%% Code and Monitor servers' info.
-record(svs, {
  code    :: pid(),
  monitor :: pid()
}).

%% Tags of an AST's node.
-record(tags, {
  this = undefined :: cuter_cerl:tag() | undefined,
  next = undefined :: cuter_cerl:tag() | undefined
}).

-type loaded_ret_atoms() :: cover_compiled | preloaded | non_existing.
-type servers() :: #svs{}.
-type ast_tags() :: #tags{}.

%%====================================================================
%% Directories
%%====================================================================

-define(RELATIVE_TMP_DIR, "temp").
-define(PYTHON_CALL, ?PYTHON_PATH ++ " -u " ++ ?PRIV ++ "/cuter_interface.py").

%%====================================================================
%% Prefixes
%%====================================================================

-define(DEPTH_PREFIX, '__conc_depth').
-define(EXECUTION_PREFIX, '__conc_prefix').
-define(SYMBOLIC_PREFIX, '__s').
-define(CONCOLIC_PREFIX_MSG, '__concm').
-define(ZIPPED_VALUE_PREFIX, '__czip').
-define(CONCOLIC_PREFIX_PDICT, '__concp').
-define(FUNCTION_PREFIX, '__cfunc').
-define(UNBOUND_VAR_PREFIX, '__uboundvar').
-define(BRANCH_TAG_PREFIX, '__branch_tag').
-define(VISITED_TAGS_PREFIX, '__visited_tags').
-define(EXECUTION_COUNTER_PREFIX, '__exec_counter').

%%====================================================================
%% Configuration parameters.
%%====================================================================

%% If enabled, we keep execution traces for debugging.
-define(DEBUG_KEEP_TRACES, debug_keep_traces).
%% If enabled, we keep the logs of the solver models and the outcomes.
-define(DEBUG_SMT, debug_smt).
%% If enabled, outputs debug logs for the solver FSM.
-define(DEBUG_SOLVER_FSM, debug_solver_fsm).

-type debug_options() :: ?DEBUG_KEEP_TRACES 
                       | ?DEBUG_SMT
                       | ?DEBUG_SOLVER_FSM
                       .

%% Sets the working directory for CutEr.
-define(WORKING_DIR, working_dir).
%% Sets the timeout for the Z3 SMT solver in seconds.
-define(Z3_TIMEOUT, z3_timeout).
%% If enabled, reports the collected metrics.
-define(REPORT_METRICS, report_metrics).
%% Selects the search strategy for the scheduler.
-define(STRATEGY, strategy).
%% If enabled, we compute and report the achieved code coverage.
-define(CALCULATE_COVERAGE, coverage).
%% If enabled, we disable the pattern matching compilation optimization.
-define(DISABLE_PMATCH, disable_pmatch).
%% If enabled, we suppresses the reporting of unsupported MFAs.
-define(SUPPRESS_UNSUPPORTED_MFAS, suppress_unsupported).
%% If enabled, we disable the normalization of types and specs.
-define(DISABLE_TYPE_NORMALIZATION, disable_type_normalization).
%% If enabled, we report the erroneous inputs in a sorted order.
-define(SORTED_ERRORS, sorted_errors).
%% Set the verbosity level for reporting.
-define(VERBOSE_EXECUTION, verbose_execution).
-define(FULLY_VERBOSE_EXECUTION, fully_verbose_execution).
-define(VERBOSITY_LEVEL, verbosity_level).
%% If set, loads the whitelisted MFAs from the file with the given path.
-define(WHITELISTED_MFA_PATH, whitelist).
-define(WHITELISTED_MFAS, whitelisted_mfas).
%% Sets the number of concurrent solver processes.
-define(NUM_SOLVERS, number_of_solvers).
%% Sets the number of concurrent concolic execution processes.
-define(NUM_POLLERS, number_of_pollers).
%% Prune safe paths.
-define(PRUNE_SAFE, prune_safe).

-type runtime_options() :: {?Z3_TIMEOUT, pos_integer()} 
                         | ?REPORT_METRICS
                         | {?STRATEGY, atom()}
                         | ?CALCULATE_COVERAGE
                         | ?DISABLE_PMATCH
                         | ?SUPPRESS_UNSUPPORTED_MFAS
                         | ?DISABLE_TYPE_NORMALIZATION
                         | ?SORTED_ERRORS
                         | ?VERBOSE_EXECUTION
                         | ?FULLY_VERBOSE_EXECUTION
                         | {?WHITELISTED_MFA_PATH, file:filename()}
                         | {?NUM_SOLVERS, pos_integer()}
                         | {?NUM_POLLERS, pos_integer()}
                         | {?WORKING_DIR, file:filename()}
                         .

%%====================================================================
%% Flags & Default Values
%%====================================================================

-define(LOGGING_FLAG, ok).
-define(LOG_UNSUPPORTED_MFAS, ok).
%%-define(VERBOSE_FILE_DELETION, ok).
%%-define(VERBOSE_MERGING, ok).
-define(USE_SPECS, ok).

%%====================================================================
%% OpCodes for types in JSON objects
%%====================================================================

-define(JSON_TYPE_ANY, 0).
-define(JSON_TYPE_INT, 1).
-define(JSON_TYPE_FLOAT, 2).
-define(JSON_TYPE_ATOM, 3).
-define(JSON_TYPE_LIST, 4).
-define(JSON_TYPE_TUPLE, 5).
-define(JSON_TYPE_PID, 6).
-define(JSON_TYPE_REF, 7).
-define(JSON_TYPE_BITSTRING, 8).
-define(JSON_TYPE_FUN, 9).

%%====================================================================
%% OpCodes for the commands to the solver
%%====================================================================

-define(JSON_CMD_LOAD_TRACE_FILE, 1).
-define(JSON_CMD_SOLVE, 2).
-define(JSON_CMD_GET_MODEL, 3).
-define(JSON_CMD_ADD_AXIOMS, 4).
-define(JSON_CMD_FIX_VARIABLE, 5).
-define(JSON_CMD_RESET_SOLVER, 6).
-define(JSON_CMD_STOP, 42).

%%====================================================================
%% OpCodes for constraint types
%%====================================================================

-define(CONSTRAINT_TRUE_REPR, 84).   %% $T
-define(CONSTRAINT_FALSE_REPR, 70).  %% $F

%%====================================================================
%% OpCodes of constraints & built-in operations
%%====================================================================

%% Empty tag ID
-define(EMPTY_TAG_ID, 0).
