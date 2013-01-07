-module(conc_lib).

-include("conc_lib.hrl").

%% External exported functions
-export([new_environment/0, add_binding/3, is_bound/2, get_value/2,
  add_mappings_to_environment/2, is_bif/3]).

%% External exported types
-export_type([environment/0, semantic_var/0, semantic_value/0]).

%% Type definitions
-type environment() :: orddict:orddict().
-type semantic_var() :: cerl:var_name().
-type semantic_value() :: term() | #valuelist{}.


%%====================================================================
%% External exports
%%====================================================================

%% Creates a new empty environment
-spec new_environment() -> environment().
new_environment() -> 
  orddict:new().
  
%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(semantic_var(), semantic_value(), environment()) -> environment().
add_binding(Var, Val, Env) ->
  orddict:store(Var, Val, Env).
  
%% Checks if Var is bound in the environment
-spec is_bound(semantic_var(), environment()) -> boolean().
is_bound(Var, Environment) ->
  orddict:is_key(Var, Environment).
  
%% Gets the Value of a bound Variable
%% Returns {ok, Value} if Var is bound,
%% or error if Var is unbound.
-spec get_value(semantic_var(), environment()) -> semantic_value().
get_value(Var, Environment) ->
  try orddict:fetch(Var, Environment) of
    Val -> {ok, Val}
  catch
    error:_Error -> error
  end.
  
%% Add new mappings to environment
%% Mappings may be a deeply nested list
-spec add_mappings_to_environment([{semantic_var(), semantic_value()}], environment()) -> environment().
add_mappings_to_environment([], Env) ->
  Env;
add_mappings_to_environment([M | Ms], Env)
  when is_list(M) ->
    NEnv = add_mappings_to_environment(M, Env),
    add_mappings_to_environment(Ms, NEnv);
add_mappings_to_environment([{Var, Val} | Ms], Env) ->
  NEnv = add_binding(Var, Val, Env),
  add_mappings_to_environment(Ms, NEnv).
  
%% Returns true if an MFA is an Erlang BIF
is_bif(erlang, _F, _A)    -> true;
is_bif(net_kernel, dflag_unicode_io, 1) -> true;
is_bif(lists, member, 2)  -> true;
is_bif(lists, reverse, 2) -> true;
is_bif(lists, keymember, 3) -> true;
is_bif(lists, keysearch, 3) -> true;
is_bif(lists, keyfind, 3) -> true;
is_bif(_M, _F, _A) -> false.
