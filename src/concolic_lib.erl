%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_lib).

%% External exported functions
-export([new_environment/0, add_binding/3, is_bound/2, get_value/2,
         bind_parameters/3, add_mappings_to_environment/2, is_bif/1,
         get_signedness/1, get_endianess/1]).

%% External exported types
-export_type([environment/0, semantic_var/0, semantic_value/0]).

-include_lib("compiler/src/core_parse.hrl").

%% Environments are rather small so orddict is more efficient
-opaque environment()  :: orddict:orddict().
-type semantic_var()   :: cerl:var_name().
-type semantic_value() :: concolic_eval:valuelist() | term().

%%====================================================================
%% External exports
%%====================================================================

%% ------------------------------------------------------------------
%% Environment Functions
%% ------------------------------------------------------------------

%% Creates a new empty environment
-spec new_environment() -> environment().
  
new_environment() -> orddict:new().
  
%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(semantic_var(), semantic_value(), environment()) -> environment().

add_binding(Var, Val, Env) -> orddict:store(Var, Val, Env).
  
%% Checks if Var is bound in the environment
-spec is_bound(semantic_var(), environment()) -> boolean().
  
is_bound(Var, Environment) -> orddict:is_key(Var, Environment).
  
%% Returns the value of a variable
-spec get_value(semantic_var(), environment()) -> {'ok', semantic_value()} | 'error'.
  
get_value(Var, Environment) ->
  try orddict:fetch(Var, Environment) of
    Val -> {ok, Val}
  catch
    error:_Error -> error
  end.
  
%% Binds the parameters of a function to their actual values
-spec bind_parameters([semantic_value()], [semantic_var()], environment()) -> environment().
  
bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var|Vars], Env) ->
  NewEnv = add_binding(Var#c_var.name, Arg, Env),
  bind_parameters(Args, Vars, NewEnv).
  
%% Add new mappings to the environment
-spec add_mappings_to_environment([concolic_symbolic:mapping()], environment()) -> environment().

add_mappings_to_environment(Ms, Env) ->
  F = fun({Var, Val}, E) -> add_binding(Var, Val, E) end,
  lists:foldl(F, Env, Ms).

%% ------------------------------------------------------------------
%% Various Helpful Functions
%% ------------------------------------------------------------------

%% Returns the type of signedness from a list of options
-spec get_signedness([bin_lib:bflag(), ...]) -> bin_lib:bsign().

get_signedness([unsigned | _Fls]) -> unsigned;
get_signedness([signed | _Fls]) -> signed;
get_signedness([_Fl | Fls]) -> get_signedness(Fls).

%% Returns the type of endianess from a list of options
-spec get_endianess([bin_lib:bflag(), ...]) -> bin_lib:bend().
  
get_endianess([big | _Fls]) -> big;
get_endianess([little | _Fls]) -> little;
get_endianess([native | _Fls]) -> native;
get_endianess([_Fl | Fls]) -> get_endianess(Fls).
  
%% TODO
%% BIFs I found during testing, may be more out there
%% Returns true if an MFA is an Erlang BIF
-spec is_bif(mfa()) -> boolean().

%% Module erlang
is_bif({erlang, _F, _A}) -> true;
%% Module beam_asm 
%% XXX Not BIF but with unsupported primops
is_bif({beam_asm, _F, _A}) -> true;
%% Module beam_lib
%% XXX Not BIF but with unsupported primops
is_bif({beam_lib, _F, _A}) -> true;
%% Module binary
is_bif({binary, compile_pattern, 1}) -> true;
is_bif({binary, match, 2}) -> true;
is_bif({binary, match, 3}) -> true;
is_bif({binary, matches, 2}) -> true;
is_bif({binary, matches, 3}) -> true;
is_bif({binary, longest_common_prefix, 1}) -> true;
is_bif({binary, longest_common_suffix, 1}) -> true;
is_bif({binary, first, 1}) -> true;
is_bif({binary, last, 1}) -> true;
is_bif({binary, at, 2}) -> true;
is_bif({binary, part, 2}) -> true;
is_bif({binary, part, 3}) -> true;
is_bif({binary, bin_to_list, 1}) -> true;
is_bif({binary, bin_to_list, 2}) -> true;
is_bif({binary, bin_to_list, 3}) -> true;
is_bif({binary, list_to_bin, 1}) -> true;
is_bif({binary, copy, 1}) -> true;
is_bif({binary, copy, 2}) -> true;
is_bif({binary, referenced_byte_size, 1}) -> true;
is_bif({binary, decode_unsigned, 1}) -> true;
is_bif({binary, decode_unsigned, 2}) -> true;
%% Module epp
%% XXX Not BIF but with unsupported primops
is_bif({epp, _F, _A}) -> true;
%% Module ets
is_bif({ets, all, 0}) -> true;
is_bif({ets, new, 2}) -> true;
is_bif({ets, delete, 1}) -> true;
is_bif({ets, delete, 2}) -> true;
is_bif({ets, first, 1}) -> true;
is_bif({ets, info, 1}) -> true;
is_bif({ets, info, 2}) -> true;
is_bif({ets, safe_fixtable, 2}) -> true;
is_bif({ets, lookup, 2}) -> true;
is_bif({ets, lookup_element, 3}) -> true;
is_bif({ets, insert, 2}) -> true;
is_bif({ets, is_compiles_ms, 1}) -> true;
is_bif({ets, last, 1}) -> true;
is_bif({ets, member, 2}) -> true;
is_bif({ets, next, 2}) -> true;
is_bif({ets, prev, 2}) -> true;
is_bif({ets, rename, 2}) -> true;
is_bif({ets, slot, 2}) -> true;
is_bif({ets, match, 1}) -> true;
is_bif({ets, match, 2}) -> true;
is_bif({ets, match, 3}) -> true;
is_bif({ets, match_object, 1}) -> true;
is_bif({ets, match_object, 2}) -> true;
is_bif({ets, match_object, 3}) -> true;
is_bif({ets, match_spec_compile, 1}) -> true;
is_bif({ets, match_spec_run_r, 3}) -> true;
is_bif({ets, select, 1}) -> true;
is_bif({ets, select, 2}) -> true;
is_bif({ets, select, 3}) -> true;
is_bif({ets, select_count, 2}) -> true;
is_bif({ets, select_reverse, 1}) -> true;
is_bif({ets, select_reverse, 2}) -> true;
is_bif({ets, select_reverse, 3}) -> true;
is_bif({ets, select_delete, 2}) -> true;
is_bif({ets, setopts, 2}) -> true;
is_bif({ets, update_counter, 3}) -> true;
is_bif({ets, update_element, 3}) -> true;
%% Module file
is_bif({file, native_name_encoding, 0}) -> true;
%% Module lists
is_bif({lists, member, 2})  -> true;
is_bif({lists, reverse, 2}) -> true;
is_bif({lists, keymember, 3}) -> true;
is_bif({lists, keysearch, 3}) -> true;
is_bif({lists, keyfind, 3}) -> true;
%% Module math
is_bif({math, pi, 0}) -> false;
is_bif({math, _F, _A}) -> true;
%% Module net_kernel
is_bif({net_kernel, dflag_unicode_io, 1}) -> true;
%% Module os
is_bif({os, getenv, 0}) -> true;
is_bif({os, getenv, 1}) -> true;
is_bif({os, getpid, 0}) -> true;
is_bif({os, putenv, 2}) -> true;
is_bif({os, timestamp, 0}) -> true;
%% Rest MFAs are not BIFs
is_bif({_M, _F, _A}) -> false.


