-module(conc_eval_bindings).

%% External exported functions
-export([new_environment/0, get_value/2, add_binding/3,
  remove_binding/2, is_bound/2]).
-export_type([environment/0, binding_var/0, binding_value/0]).

%% External exported types
-opaque environment() :: orddict:orddict().
-type binding_var() :: cerl:var_name().
-type binding_value() :: term().


%%====================================================================
%% External exports
%%====================================================================

%% Creates a new empty environment
-spec new_environment() -> environment().
new_environment() ->
  orddict:new().
  
%% Checks if Var is bound in the environment
-spec is_bound(binding_var(), environment()) -> boolean().
is_bound(Var, Environment) ->
  orddict:is_key(Var, Environment).
  
%% Get the Value of a bound Variable
%% Returns {ok, Value} if Var is bound,
%% or error if Var is unbound.
-spec get_value(binding_var(), environment()) -> binding_value().
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
-spec add_binding(binding_var(), binding_value(), environment()) -> environment().
add_binding(Var, Value, Environment) ->
  case is_bound(Var, Environment) of
    true ->
      error;
    false ->
      NewEnvironment = orddict:store(Var, Value, Environment),
      {ok, NewEnvironment}
  end.

%% Removes a binding from the environment
-spec remove_binding(binding_var(), environment()) -> environment().
remove_binding(Var, Environment) ->
  orddict:erase(Var, Environment).
