-module(conc_lib).

-include("conc_lib.hrl").

%% External exported functions
-export([new_environment/0, get_value/2, add_binding/3,
  remove_binding/2, is_bound/2, term_to_semantic/1, semantic_to_term/1,
  terms_to_semantics/1, semantics_to_terms/1, add_mappings_to_environment/2]).

%% External exported types
-export_type([environment/0, semantic_var/0, semantic_value/0]).

%% Type definitions
-type environment() :: orddict:orddict().
-type semantic_var() :: cerl:var_name().
-type semantic_value() :: #semantic{}.


%%====================================================================
%% External exports
%%====================================================================

%% Creates a new empty environment
-spec new_environment() -> environment().
new_environment() -> 
  orddict:new().
  
%% Checks if Var is bound in the environment
-spec is_bound(semantic_var(), environment()) -> boolean().
is_bound(Var, Environment) ->
  orddict:is_key(Var, Environment).
  
%% Get the Value of a bound Variable
%% Returns {ok, Value} if Var is bound,
%% or error if Var is unbound.
-spec get_value(semantic_var(), environment()) -> semantic_value().
get_value(Var, Environment) ->
  case is_bound(Var, Environment) of
    true ->
      {ok, orddict:fetch(Var, Environment)};
    false ->
      error
  end.

%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(semantic_var(), semantic_value(), environment()) -> environment().
add_binding(Var, Value, Environment) ->
  orddict:store(Var, Value, Environment).
  
%% Add new mappings to environment
-spec add_mappings_to_environment([{semantic_var(), semantic_value()}], environment()) -> environment().
add_mappings_to_environment([], Env) ->
  Env;
add_mappings_to_environment([{Var, Value}|Ms], Env) ->
  NEnv = add_binding(Var, Value, Env),
  add_mappings_to_environment(Ms, NEnv).

%% Removes a binding from the environment
-spec remove_binding(semantic_var(), environment()) -> environment().
remove_binding(Var, Environment) ->
  orddict:erase(Var, Environment).
  
%% Functions to wrap terms into semantic values
-spec terms_to_semantics([term()]) -> [semantic_value()].
terms_to_semantics(Terms) ->
  terms_to_semantics(Terms, []).
  
-spec term_to_semantic(term()) -> semantic_value().
term_to_semantic(Term) ->
  #semantic{value=Term, degree=1}.
  
%% Functions to unwrap terms from semantic values
-spec semantics_to_terms([semantic_value()]) -> [term()].
semantics_to_terms(Semantics) ->
  semantics_to_terms(Semantics, []).

-spec semantic_to_term(semantic_value()) -> term().
semantic_to_term(Semantic) ->
  Semantic#semantic.value.  
  
%%====================================================================
%% Internal functions
%%====================================================================
  
-spec terms_to_semantics([term()], [semantic_value()]) -> [semantic_value()].
terms_to_semantics([], Acc) ->
  lists:reverse(Acc);
terms_to_semantics([Term|Terms], Acc) ->
  SemanticTerm = term_to_semantic(Term),
  terms_to_semantics(Terms, [SemanticTerm|Acc]).
  
-spec semantics_to_terms([semantic_value()], [term()]) -> [term()].
semantics_to_terms([], Acc) ->
  lists:reverse(Acc);
semantics_to_terms([Semantic|Semantics], Acc) ->
  Term = semantic_to_term(Semantic),
  semantics_to_terms(Semantics, [Term|Acc]).
