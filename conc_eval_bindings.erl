-module(conc_eval_bindings).

-include("conc_eval_bindings.hrl").

%% External exported functions
-export([new_environment/0, get_value/2, add_binding/3,
  remove_binding/2, is_bound/2, term_to_semantic/1, semantic_to_term/1,
  terms_to_semantics/1, semantics_to_terms/1]).
-export_type([environment/0, semantic_var/0, semantic_value/0]).

%% External exported types
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
%% Returns {ok, NewEnvironment} if Var was unbound,
%% or error if Var was bound.
-spec add_binding(semantic_var(), semantic_value(), environment()) -> environment().
add_binding(Var, Value, Environment) ->
  case is_bound(Var, Environment) of
    true ->
      error;
    false ->
      NewEnvironment = orddict:store(Var, Value, Environment),
      {ok, NewEnvironment}
  end.

%% Removes a binding from the environment
-spec remove_binding(semantic_var(), environment()) -> environment().
remove_binding(Var, Environment) ->
  orddict:erase(Var, Environment).
  
%% Functions to wrap terms into semantic values
terms_to_semantics(Terms) ->
  terms_to_semantics(Terms, []).
  
terms_to_semantics([], Acc) ->
  lists:reverse(Acc);
terms_to_semantics([Term|Terms], Acc) ->
  SemanticTerm = term_to_semantic(Term),
  terms_to_semantics(Terms, [SemanticTerm|Acc]).
  
term_to_semantic(Term) ->
  #semantic{value=Term, degree=1}.
  
%% Functions to unwrap terms from sem.antic values
semantics_to_terms(Semantics) ->
  semantics_to_terms(Semantics, []).
  
semantics_to_terms([], Acc) ->
  lists:reverse(Acc);
semantics_to_terms([Semantic|Semantics], Acc) ->
  Term = semantic_to_term(Semantic),
  semantics_to_terms(Semantics, [Term|Acc]).
  
semantic_to_term(Semantic) ->
  Semantic#semantic.value.
