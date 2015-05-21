%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_env).

-export([new_environment/0, add_binding/3, bind_parameters/3,
	 get_value/2, add_mappings_to_environment/2]).

-include_lib("compiler/src/core_parse.hrl").

%% External exported types
-export_type([environment/0]).

%% Environments are rather small so orddict is more efficient
-type environment() :: orddict:orddict().
-type cvar() :: cerl:var_name().
-type cval() :: cuter_eval:valuelist() | cuter_eval:value().


%% Creates a new empty environment
-spec new_environment() -> environment().
new_environment() -> orddict:new().

%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(cvar(), cval(), environment()) -> environment().
add_binding(Var, Val, Env) -> orddict:store(Var, Val, Env).

%% Returns the value of a variable
-spec get_value(cvar(), environment()) -> {ok, cval()} | error.
get_value(Var, Env) ->
  try orddict:fetch(Var, Env) of
    Val -> {ok, Val}
  catch
    error:_Error -> error
  end.

%% Binds the parameters of a function to their actual values
-spec bind_parameters([cval()], [cvar()], environment()) -> environment().
bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var|Vars], Env) ->
  bind_parameters(Args, Vars, add_binding(Var#c_var.name, Arg, Env)).

%% Add new mappings to the environment
-spec add_mappings_to_environment([cuter_symbolic:mapping()], environment()) -> environment().
add_mappings_to_environment(Ms, Env) ->
  F = fun({Var, Val}, E) -> add_binding(Var, Val, E) end,
  lists:foldl(F, Env, Ms).
