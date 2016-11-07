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
%% Flags & Default Values
%%====================================================================

-define(LOGGING_FLAG, ok).
-define(DELETE_TRACE, ok).
-define(LOG_UNSUPPORTED_MFAS, ok).
%%-define(VERBOSE_SCHEDULER, ok).
%%-define(VERBOSE_FILE_DELETION, ok).
%%-define(VERBOSE_SOLVING, ok).
%%-define(VERBOSE_MERGING, ok).
%%-define(VERBOSE_REPORTING, ok).
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
