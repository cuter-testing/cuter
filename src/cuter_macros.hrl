%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------

%%====================================================================
%% Types
%%====================================================================

-record(svs, {
  code    :: pid(),
  monitor :: pid()
}).

-type loaded_ret_atoms() :: cover_compiled | preloaded | non_existing.
-type logs() :: [{atom(), any()}].
-type servers() :: #svs{}.

%%====================================================================
%% Directories
%%====================================================================

-define(TRACE_DIR(BaseDir), BaseDir ++ "/traces").
-define(CORE_DIR(BaseDir), BaseDir ++ "/core").
-define(TMP_DIR, "temp").

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

%%====================================================================
%% Flags
%%====================================================================

-define(LOGGING_FLAG, ok).

%%====================================================================
%% Various OpCodes
%%====================================================================

-define(CONSTRAINT_TRUE, 1).
-define(CONSTRAINT_FALSE, 2).
-define(NOT_CONSTRAINT, 3).

-define(OP_PARAMS, 1).
-define(OP_SPEC, 2).
-define(OP_GUARD_TRUE, 3).
-define(OP_GUARD_FALSE, 4).
-define(OP_MATCH_EQUAL_TRUE, 5).
-define(OP_MATCH_EQUAL_FALSE, 6).
-define(OP_TUPLE_SZ, 7).
-define(OP_TUPLE_NOT_SZ, 8).
-define(OP_TUPLE_NOT_TPL, 9).
-define(OP_LIST_NON_EMPTY, 10).
-define(OP_LIST_EMPTY, 11).
-define(OP_LIST_NOT_LST, 12).
-define(OP_SPAWN, 13).
-define(OP_MSG_SEND, 14).
-define(OP_MSG_RECEIVE, 15).
-define(OP_UNFOLD_TUPLE, 16).
-define(OP_UNFOLD_LIST, 17).
-define(OP_ERLANG_HD_1, 25).
-define(OP_ERLANG_TL_1, 26).

