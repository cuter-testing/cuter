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
  

%% TODO BIFs I found during testing, may be more out there
%% Returns true if an MFA is an Erlang BIF

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


