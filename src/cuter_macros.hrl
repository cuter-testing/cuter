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
