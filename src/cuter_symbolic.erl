%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_symbolic).
-compile(export_all).

-include("cuter_macros.hrl").

-export_type([mapping/0, symbolic/0]).

-type symbolic() :: {?SYMBOLIC_PREFIX, nonempty_string()}.  %% Symbolic Variable
-type mapping()  :: {symbolic(), any()}.

%% Create a fresh symbolic variable
-spec fresh_symbolic_var() -> symbolic().
fresh_symbolic_var() -> {?SYMBOLIC_PREFIX, cuter_lib:unique_string()}.

%% Abstract a list of concrete values
-spec abstract([any()]) -> {[symbolic()], [mapping()]}.
abstract(Vs) ->
  Symbs = [fresh_symbolic_var() || _ <- lists:seq(1, erlang:length(Vs))],
  Maps = lists:zip(Symbs, Vs),
  {Symbs, Maps}.
