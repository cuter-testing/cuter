%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------

%%====================================================================
%% Prefixes
%%====================================================================

%% concolic_symbolic
-define(SYMBOLIC_PREFIX, '__s').

%% concolic_encdec, concolic_eval, concolic_tserver
-define(DEPTH_PREFIX, '__conc_depth').

%% concolic_json
-define(UNBOUND_VAR, '__any').

%% concolic_eval
-define(FUNCTION_PREFIX, '__func').
-define(CONCOLIC_PREFIX_MSG, '__concm').
-define(CONCOLIC_PREFIX_PDICT, '__concp').

%%====================================================================
%% Shared Macros
%%====================================================================

%% concolic_encdec, concolic_analyzer
-define(CONSTRAINT_TRUE_REP, 84).   %% $T
-define(CONSTRAINT_FALSE_REP, 70).  %% $F
