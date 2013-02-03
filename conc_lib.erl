-module(conc_lib).

-include("conc_lib.hrl").
-include_lib("compiler/src/core_parse.hrl").

%% External exported functions
-export([new_environment/0, add_binding/3, is_bound/2, get_value/2,
  bind_parameters/3, add_mappings_to_environment/2, is_bif/3,
  get_signedness/1, get_endianess/1]).

%% External exported types
-export_type([environment/0, semantic_var/0, semantic_value/0]).

%% Type definitions

%% Environments are rather small so orddict is more efficient
-type environment()    :: orddict:orddict().
%% cerl:var_name() :: integer() | atom() | {atom(), integer()}.
-type semantic_var()   :: cerl:var_name().
%% #valuelist{} is actually a term but this definition is more descriptive
-type semantic_value() :: #valuelist{} | term().

%%====================================================================
%% External exports
%%====================================================================

%% Creates a new empty environment
-spec new_environment() -> Env
  when Env :: environment().
  
new_environment() -> 
  orddict:new().
  
%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(Var, Value, Env) -> NewEnv
  when Var    :: semantic_var(),
       Value  :: semantic_value(),
       Env    :: environment(),
       NewEnv :: environment().
       
add_binding(Var, Val, Env) ->
  orddict:store(Var, Val, Env).
  
%% Checks if Var is bound in the environment
-spec is_bound(Var, Env) -> boolean()
  when Var :: semantic_var(),
       Env :: environment().
       
is_bound(Var, Environment) ->
  orddict:is_key(Var, Environment).
  
%% Gets the Value of a bound Variable
%% Returns {ok, Value} if Var is bound,
%% or error if Var is unbound.
-spec get_value(Var, Env) -> Value | error
  when Var   :: semantic_value(),
       Env   :: environment(),
       Value :: semantic_value().
       
get_value(Var, Environment) ->
  try orddict:fetch(Var, Environment) of
    Val -> {ok, Val}
  catch
    error:_Error -> error
  end.
  
%% Binds the parameters of a function to their actual values
-spec bind_parameters(Vals, Vars, OldEnv) -> Env
  when Vals   :: [semantic_value()],
       Vars   :: [semantic_var()],
       OldEnv :: environment(),
       Env    :: environment().

bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var|Vars], Env) ->
  NewEnv = conc_lib:add_binding(Var#c_var.name, Arg, Env),
  bind_parameters(Args, Vars, NewEnv).
  
%% Add new mappings to the environment
%% Mappings may be a deeply nested list
-spec add_mappings_to_environment(Mapps, Env) -> NewEnv
  when Mapps   :: [{Var, Value}],
         Var   :: semantic_var(),
         Value :: semantic_value(),
       Env     :: environment(),
       NewEnv  :: environment().
       
add_mappings_to_environment([], Env) ->
  Env;
add_mappings_to_environment([M | Ms], Env)
  when is_list(M) ->
    NEnv = add_mappings_to_environment(M, Env),
    add_mappings_to_environment(Ms, NEnv);
add_mappings_to_environment([{Var, Val} | Ms], Env) ->
  NEnv = add_binding(Var, Val, Env),
  add_mappings_to_environment(Ms, NEnv).
  
%% Returns the type of signedness from a list of options
-spec get_signedness(List) -> unsigned | signed
  when List :: [atom()].

get_signedness([unsigned | _Fls]) -> unsigned;
get_signedness([signed | _Fls]) -> signed;
get_signedness([_Fl | Fls]) -> get_signedness(Fls).

%% Returns the type of endianess from a list of options
-spec get_endianess(List) -> big | little | native
  when List :: [atom()].
  
get_endianess([big | _Fls]) -> big;
get_endianess([little | _Fls]) -> little;
get_endianess([native | _Fls]) -> native;
get_endianess([_Fl | Fls]) -> get_endianess(Fls).
  

%% TODO
%% BIFs I found during testing, may be more out there
%% Returns true if an MFA is an Erlang BIF

-spec is_bif(M, F, A) -> boolean()
  when M :: atom(),
       F :: atom(),
       A :: non_neg_integer().

%% Module erlang
is_bif(erlang, _F, _A)    -> true;

%% TODO Not BIF but having probs with some primops
%% Module beam_asm 
is_bif(beam_asm, _, _) -> true;

%% Module binary
is_bif(binary, compile_pattern, 1) -> true;
is_bif(binary, match, 2) -> true;
is_bif(binary, match, 3) -> true;
is_bif(binary, matches, 2) -> true;
is_bif(binary, matches, 3) -> true;
is_bif(binary, longest_common_prefix, 1) -> true;
is_bif(binary, longest_common_suffix, 1) -> true;
is_bif(binary, first, 1) -> true;
is_bif(binary, last, 1) -> true;
is_bif(binary, at, 2) -> true;
is_bif(binary, part, 2) -> true;
is_bif(binary, part, 3) -> true;
is_bif(binary, bin_to_list, 1) -> true;
is_bif(binary, bin_to_list, 2) -> true;
is_bif(binary, bin_to_list, 3) -> true;
is_bif(binary, list_to_bin, 1) -> true;
is_bif(binary, copy, 1) -> true;
is_bif(binary, copy, 2) -> true;
is_bif(binary, referenced_byte_size, 1) -> true;
is_bif(binary, decode_unsigned, 1) -> true;
is_bif(binary, decode_unsigned, 2) -> true;

% Module ets
is_bif(ets, all, 0) -> true;
is_bif(ets, new, 2) -> true;
is_bif(ets, delete, 1) -> true;
is_bif(ets, delete, 2) -> true;
is_bif(ets, first, 1) -> true;
is_bif(ets, info, 1) -> true;
is_bif(ets, info, 2) -> true;
is_bif(ets, safe_fixtable, 2) -> true;
is_bif(ets, lookup, 2) -> true;
is_bif(ets, lookup_element, 3) -> true;
is_bif(ets, insert, 2) -> true;
is_bif(ets, is_compiles_ms, 1) -> true;
is_bif(ets, last, 1) -> true;
is_bif(ets, member, 2) -> true;
is_bif(ets, next, 2) -> true;
is_bif(ets, prev, 2) -> true;
is_bif(ets, rename, 2) -> true;
is_bif(ets, slot, 2) -> true;
is_bif(ets, match, 1) -> true;
is_bif(ets, match, 2) -> true;
is_bif(ets, match, 3) -> true;
is_bif(ets, match_object, 1) -> true;
is_bif(ets, match_object, 2) -> true;
is_bif(ets, match_object, 3) -> true;
is_bif(ets, match_spec_compile, 1) -> true;
is_bif(ets, match_spec_run_r, 3) -> true;
is_bif(ets, select, 1) -> true;
is_bif(ets, select, 2) -> true;
is_bif(ets, select, 3) -> true;
is_bif(ets, select_count, 2) -> true;
is_bif(ets, select_reverse, 1) -> true;
is_bif(ets, select_reverse, 2) -> true;
is_bif(ets, select_reverse, 3) -> true;
is_bif(ets, select_delete, 2) -> true;
is_bif(ets, setopts, 2) -> true;
is_bif(ets, update_counter, 3) -> true;
is_bif(ets, update_element, 3) -> true;

%% Module file
is_bif(file, native_name_encoding, 0) -> true;

%% Module lists
is_bif(lists, member, 2)  -> true;
is_bif(lists, reverse, 2) -> true;
is_bif(lists, keymember, 3) -> true;
is_bif(lists, keysearch, 3) -> true;
is_bif(lists, keyfind, 3) -> true;

%% Module math
%% math:pi/0 is not a BIF but there is no point at interpreting it
is_bif(math, _F, _A) -> true;

%% Module net_kernel
is_bif(net_kernel, dflag_unicode_io, 1) -> true;

%% Module os
is_bif(os, getenv, 0) ->true;
is_bif(os, getenv, 1) ->true;
is_bif(os, getpid, 0) ->true;
is_bif(os, putenv, 2) -> true;
is_bif(os, timestamp, 0) -> true;

%% Rest are not BiFs
is_bif(_M, _F, _A) -> false.


