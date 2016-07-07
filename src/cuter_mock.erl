%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_mock).

-export([simulate_behaviour/3, is_whitelisted/2, parse_whitelist/1,
         get_whitelisted_mfas/1, empty_whitelist/0, overriding_modules/0,
         no_symbolic_evalution/1, is_internal_without_symbolic_representation/1]).

-export_type([whitelist/0]).

-include("include/cuter_macros.hrl").

-type entry() :: {atom(), atom(), arity()} | {atom(), atom(), '*'} | {atom(), '*', '*'}.
-type whitelist() :: sets:set(entry()).

%% BIFs I found during testing, may be more out there
%% Returns bif if an MFA is an Erlang BIF
-spec simulate_behaviour(module(), atom(), non_neg_integer()) -> bif | {ok, mfa()}.

%% erlang module
%% All overrides will be in cuter_erlang module.
simulate_behaviour(erlang, abs,             1) -> {ok, {cuter_erlang, abs,             1}};
simulate_behaviour(erlang, 'and',           2) -> {ok, {cuter_erlang, 'and',           2}};
simulate_behaviour(erlang, 'andalso',       2) -> {ok, {cuter_erlang, 'andalso',       2}};
simulate_behaviour(erlang, atom_to_list,    1) -> {ok, {cuter_erlang, atom_to_list,    1}};
simulate_behaviour(erlang, 'div',           2) -> {ok, {cuter_erlang, 'div',           2}};
simulate_behaviour(erlang, element,         2) -> {ok, {cuter_erlang, element,         2}};
simulate_behaviour(erlang, float,           1) -> {ok, {cuter_erlang, float,           1}};
simulate_behaviour(erlang, hd,              1) -> {ok, {cuter_erlang, hd,              1}};
simulate_behaviour(erlang, integer_to_list, 1) -> {ok, {cuter_erlang, integer_to_list, 1}};
simulate_behaviour(erlang, list_to_integer, 1) -> {ok, {cuter_erlang, list_to_integer, 1}};
simulate_behaviour(erlang, length,          1) -> {ok, {cuter_erlang, length,          1}};
simulate_behaviour(erlang, list_to_tuple,   1) -> {ok, {cuter_erlang, list_to_tuple,   1}};
simulate_behaviour(erlang, make_tuple,      2) -> {ok, {cuter_erlang, make_tuple,      2}};
simulate_behaviour(erlang, max,             2) -> {ok, {cuter_erlang, max,             2}};
simulate_behaviour(erlang, min,             2) -> {ok, {cuter_erlang, min,             2}};
simulate_behaviour(erlang, 'not',           1) -> {ok, {cuter_erlang, 'not',           1}};
simulate_behaviour(erlang, 'rem',           2) -> {ok, {cuter_erlang, 'rem',           2}};
simulate_behaviour(erlang, 'or',            2) -> {ok, {cuter_erlang, 'or',            2}};
simulate_behaviour(erlang, 'orelse',        2) -> {ok, {cuter_erlang, 'orelse',        2}};
simulate_behaviour(erlang, setelement,      3) -> {ok, {cuter_erlang, setelement,      3}};
simulate_behaviour(erlang, tl,              1) -> {ok, {cuter_erlang, tl,              1}};
simulate_behaviour(erlang, tuple_size,      1) -> {ok, {cuter_erlang, tuple_size,      1}};
simulate_behaviour(erlang, tuple_to_list,   1) -> {ok, {cuter_erlang, tuple_to_list,   1}};
simulate_behaviour(erlang, 'xor',           2) -> {ok, {cuter_erlang, 'xor',           2}};
simulate_behaviour(erlang, '++',            2) -> {ok, {cuter_erlang, '++',            2}};
simulate_behaviour(erlang, '--',            2) -> {ok, {cuter_erlang, '--',            2}};
simulate_behaviour(erlang, '=:=',           2) -> {ok, {cuter_erlang, '=:=',           2}};
simulate_behaviour(erlang, '=/=',           2) -> {ok, {cuter_erlang, '=/=',           2}};
simulate_behaviour(erlang, '==',            2) -> {ok, {cuter_erlang, '==',            2}};
simulate_behaviour(erlang, '/=',            2) -> {ok, {cuter_erlang, '/=',            2}};
simulate_behaviour(erlang, '<',             2) -> {ok, {cuter_erlang, '<',             2}};
simulate_behaviour(erlang, '=<',            2) -> {ok, {cuter_erlang, '=<',            2}};
simulate_behaviour(erlang, '>',             2) -> {ok, {cuter_erlang, '>',             2}};
simulate_behaviour(erlang, '>=',            2) -> {ok, {cuter_erlang, '>=',            2}};
simulate_behaviour(erlang, '+',             2) -> {ok, {cuter_erlang, '+',             2}};
simulate_behaviour(erlang, '-',             2) -> {ok, {cuter_erlang, '-',             2}};
simulate_behaviour(erlang, '*',             2) -> {ok, {cuter_erlang, '*',             2}};
simulate_behaviour(erlang, '/',             2) -> {ok, {cuter_erlang, '/',             2}};
simulate_behaviour(erlang, is_binary,       1) -> {ok, {cuter_erlang, is_binary,       1}};
simulate_behaviour(erlang, bit_size,        1) -> {ok, {cuter_erlang, bit_size,        1}};
simulate_behaviour(erlang, byte_size,       1) -> {ok, {cuter_erlang, byte_size,       1}};
simulate_behaviour(erlang, 'bsl',           2) -> {ok, {cuter_erlang, 'bsl',           2}};
simulate_behaviour(erlang, 'bsr',           2) -> {ok, {cuter_erlang, 'bsr',           2}};
simulate_behaviour(erlang, 'bnot',          1) -> {ok, {cuter_erlang, 'bnot',          1}};
simulate_behaviour(erlang, trunc,           1) -> {ok, {cuter_erlang, trunc,           1}};
simulate_behaviour(erlang, _F, _A)        -> bif;

%% cuter_erlang module
simulate_behaviour(cuter_erlang, F, A) ->
  MFA = {cuter_erlang, F, A},
  case gb_sets:is_member(MFA, ?SUPPORTED_MFAS) orelse is_internal_without_symbolic_representation(MFA) of
    true  -> bif;
    false -> {ok, MFA}
  end;

%% Module beam_asm 
%% XXX Not BIF but with unsupported primops
simulate_behaviour(beam_asm, _F, _A) -> bif;
%% Module beam_lib
%% XXX Not BIF but with unsupported primops
simulate_behaviour(beam_lib, _F, _A) -> bif;
%% Module binary
simulate_behaviour(binary, compile_pattern, 1) -> bif;
simulate_behaviour(binary, match, 2) -> bif;
simulate_behaviour(binary, match, 3) -> bif;
simulate_behaviour(binary, matches, 2) -> bif;
simulate_behaviour(binary, matches, 3) -> bif;
simulate_behaviour(binary, longest_common_prefix, 1) -> bif;
simulate_behaviour(binary, longest_common_suffix, 1) -> bif;
simulate_behaviour(binary, first, 1) -> bif;
simulate_behaviour(binary, last, 1) -> bif;
simulate_behaviour(binary, at, 2) -> bif;
simulate_behaviour(binary, part, 2) -> bif;
simulate_behaviour(binary, part, 3) -> bif;
simulate_behaviour(binary, bin_to_list, 1) -> bif;
simulate_behaviour(binary, bin_to_list, 2) -> bif;
simulate_behaviour(binary, bin_to_list, 3) -> bif;
simulate_behaviour(binary, list_to_bin, 1) -> bif;
simulate_behaviour(binary, copy, 1) -> bif;
simulate_behaviour(binary, copy, 2) -> bif;
simulate_behaviour(binary, referenced_byte_size, 1) -> bif;
simulate_behaviour(binary, decode_unsigned, 1) -> bif;
simulate_behaviour(binary, decode_unsigned, 2) -> bif;
%% Module epp
%% XXX Not BIF but with unsupported primops
simulate_behaviour(epp, _F, _A) -> bif;
%% Module crypto
simulate_behaviour(crypto, md5, 1) -> bif;
simulate_behaviour(crypto, info_lib, 0) -> bif;
simulate_behaviour(crypto, hmac_init, 2) -> bif;
simulate_behaviour(crypto, hmac_final, 1) -> bif;
simulate_behaviour(crypto, hmac_final_n, 2) -> bif;
simulate_behaviour(crypto, rand_bytes, 1) -> bif;
simulate_behaviour(crypto, strong_rand_bytes_nif, 1) -> bif;
simulate_behaviour(crypto, rand_bytes, 3) -> bif;
simulate_behaviour(crypto, rand_seed_nif, 1) -> bif;
simulate_behaviour(crypto, md5_init, 0) -> bif;
simulate_behaviour(crypto, md5_update, 2) -> bif;
simulate_behaviour(crypto, md5_final, 1) -> bif;
simulate_behaviour(crypto, md4, 1) -> bif;
simulate_behaviour(crypto, md4_init, 0) -> bif;
simulate_behaviour(crypto, md4_update, 2) -> bif;
simulate_behaviour(crypto, md4_final, 1) -> bif;
simulate_behaviour(crypto, ripemd160, 1) -> bif;
simulate_behaviour(crypto, ripemd160_init, 0) -> bif;
simulate_behaviour(crypto, ripemd160_update, 2) -> bif;
simulate_behaviour(crypto, ripemd160_final, 1) -> bif;
simulate_behaviour(crypto, sha, 1) -> bif;
simulate_behaviour(crypto, sha_init, 0) -> bif;
simulate_behaviour(crypto, sha_update, 2) -> bif;
simulate_behaviour(crypto, sha_final, 1) -> bif;
simulate_behaviour(crypto, sha224_nif, 1) -> bif;
simulate_behaviour(crypto, sha224_init_nif, 0) -> bif;
simulate_behaviour(crypto, sha224_update_nif, 2) -> bif;
simulate_behaviour(crypto, sha224_final_nif, 1) -> bif;
simulate_behaviour(crypto, sha256_nif, 1) -> bif;
simulate_behaviour(crypto, sha256_init_nif, 0) -> bif;
simulate_behaviour(crypto, sha256_update_nif, 2) -> bif;
simulate_behaviour(crypto, sha256_final_nif, 1) -> bif;
simulate_behaviour(crypto, sha384_nif, 1) -> bif;
simulate_behaviour(crypto, sha384_init_nif, 0) -> bif;
simulate_behaviour(crypto, sha384_update_nif, 2) -> bif;
simulate_behaviour(crypto, sha384_final_nif, 1) -> bif;
simulate_behaviour(crypto, sha512_nif, 1) -> bif;
simulate_behaviour(crypto, sha512_init_nif, 0) -> bif;
simulate_behaviour(crypto, sha512_update_nif, 2) -> bif;
simulate_behaviour(crypto, sha512_final_nif, 1) -> bif;
simulate_behaviour(crypto, do_hmac_update, 2) -> bif;
simulate_behaviour(crypto, md5_mac_n, 3) -> bif;
simulate_behaviour(crypto, sha_mac_n, 3) -> bif;
simulate_behaviour(crypto, sha224_mac_nif, 3) -> bif;
simulate_behaviour(crypto, sha256_mac_nif, 3) -> bif;
simulate_behaviour(crypto, sha384_mac_nif, 3) -> bif;
simulate_behaviour(crypto, sha512_mac_nif, 3) -> bif;
simulate_behaviour(crypto, des_ecb_crypt, 3) -> bif;
simulate_behaviour(crypto, des_ede3_cbc_crypt, 6) -> bif;
simulate_behaviour(crypto, des_ede3_cfb_crypt_nif, 6) -> bif;
simulate_behaviour(crypto, bf_ecb_crypt, 3) -> bif;
simulate_behaviour(crypto, bf_cbc_crypt, 4) -> bif;
simulate_behaviour(crypto, bf_cfb64_crypt, 4) -> bif;
simulate_behaviour(crypto, blowfish_ofb64_encrypt, 3) -> bif;
simulate_behaviour(crypto, aes_cfb_8_crypt, 4) -> bif;
simulate_behaviour(crypto, aes_cfb_128_crypt, 4) -> bif;
simulate_behaviour(crypto, des_cbc_crypt, 4) -> bif;
simulate_behaviour(crypto, des_cfb_crypt, 4) -> bif;
simulate_behaviour(crypto, aes_cbc_crypt, 4) -> bif;
simulate_behaviour(crypto, aes_ige_crypt_nif, 4) -> bif;
simulate_behaviour(crypto, aes_ctr_encrypt, 3) -> bif;
simulate_behaviour(crypto, aes_ctr_decrypt, 3) -> bif;
simulate_behaviour(crypto, aes_ctr_stream_encrypt, 2) -> bif;
simulate_behaviour(crypto, aes_ctr_stream_decrypt, 2) -> bif;
simulate_behaviour(crypto, rc4_encrypt, 2) -> bif;
simulate_behaviour(crypto, rc4_set_key, 1) -> bif;
simulate_behaviour(crypto, rc4_encrypt_with_state, 2) -> bif;
simulate_behaviour(crypto, rc2_cbc_crypt, 4) -> bif;
simulate_behaviour(crypto, srp_host_secret_nif, 5) -> bif;
simulate_behaviour(crypto, rsa_sign_nif, 3) -> bif;
simulate_behaviour(crypto, dss_sign_nif, 3) -> bif;
simulate_behaviour(crypto, ecdsa_sign_nif, 4) -> bif;
simulate_behaviour(crypto, dss_verify_nif, 4) -> bif;
simulate_behaviour(crypto, rsa_verify_nif, 4) -> bif;
simulate_behaviour(crypto, ecdsa_verify_nif, 5) -> bif;
simulate_behaviour(crypto, dh_check, 1) -> bif;
simulate_behaviour(crypto, dh_generate_key_nif, 3) -> bif;
simulate_behaviour(crypto, dh_compute_key_nif, 3) -> bif;
simulate_behaviour(crypto, ec_key_generate, 1) -> bif;
simulate_behaviour(crypto, ecdh_compute_key_nif, 3) -> bif;
simulate_behaviour(crypto, do_exor, 2) -> bif;
simulate_behaviour(crypto, algorithms, 0) -> bif;
simulate_behaviour(crypto, rsa_public_crypt, 4) -> bif;
simulate_behaviour(crypto, rsa_private_crypt, 4) -> bif;
simulate_behaviour(crypto, strong_rand_mpint_nif, 3) -> bif;
simulate_behaviour(crypto, mod_exp_nif, 5) -> bif;
simulate_behaviour(crypto, rand_uniform, 2) -> bif;  %% Depends on NIF crypto:rand_uniform_nif/2
%% Module ets
simulate_behaviour(ets, all, 0) -> bif;
simulate_behaviour(ets, new, 2) -> bif;
simulate_behaviour(ets, delete, 1) -> bif;
simulate_behaviour(ets, delete, 2) -> bif;
simulate_behaviour(ets, first, 1) -> bif;
simulate_behaviour(ets, info, 1) -> bif;
simulate_behaviour(ets, info, 2) -> bif;
simulate_behaviour(ets, safe_fixtable, 2) -> bif;
simulate_behaviour(ets, lookup, 2) -> bif;
simulate_behaviour(ets, lookup_element, 3) -> bif;
simulate_behaviour(ets, insert, 2) -> bif;
simulate_behaviour(ets, is_compiles_ms, 1) -> bif;
simulate_behaviour(ets, last, 1) -> bif;
simulate_behaviour(ets, member, 2) -> bif;
simulate_behaviour(ets, next, 2) -> bif;
simulate_behaviour(ets, prev, 2) -> bif;
simulate_behaviour(ets, rename, 2) -> bif;
simulate_behaviour(ets, slot, 2) -> bif;
simulate_behaviour(ets, match, 1) -> bif;
simulate_behaviour(ets, match, 2) -> bif;
simulate_behaviour(ets, match, 3) -> bif;
simulate_behaviour(ets, match_object, 1) -> bif;
simulate_behaviour(ets, match_object, 2) -> bif;
simulate_behaviour(ets, match_object, 3) -> bif;
simulate_behaviour(ets, match_spec_compile, 1) -> bif;
simulate_behaviour(ets, match_spec_run_r, 3) -> bif;
simulate_behaviour(ets, select, 1) -> bif;
simulate_behaviour(ets, select, 2) -> bif;
simulate_behaviour(ets, select, 3) -> bif;
simulate_behaviour(ets, select_count, 2) -> bif;
simulate_behaviour(ets, select_reverse, 1) -> bif;
simulate_behaviour(ets, select_reverse, 2) -> bif;
simulate_behaviour(ets, select_reverse, 3) -> bif;
simulate_behaviour(ets, select_delete, 2) -> bif;
simulate_behaviour(ets, setopts, 2) -> bif;
simulate_behaviour(ets, update_counter, 3) -> bif;
simulate_behaviour(ets, update_element, 3) -> bif;
%% Module file
simulate_behaviour(file, native_name_encoding, 0) -> bif;
%% Module lists
simulate_behaviour(lists, member, 2)  -> {ok, {cuter_erlang, member, 2}};
simulate_behaviour(lists, reverse, 2) -> {ok, {cuter_erlang, reverse, 2}};
simulate_behaviour(lists, keymember, 3) -> bif;
simulate_behaviour(lists, keysearch, 3) -> bif;
simulate_behaviour(lists, keyfind, 3) -> {ok, {cuter_erlang, keyfind, 3}};
%% Module math
simulate_behaviour(math, pi, 0) -> {ok, {math, pi, 0}};
simulate_behaviour(math, _F, _A) -> bif;
%% Module net_kernel
simulate_behaviour(net_kernel, dflag_unicode_io, 1) -> bif;
%% Module os
simulate_behaviour(os, getenv, 0) -> bif;
simulate_behaviour(os, getenv, 1) -> bif;
simulate_behaviour(os, getpid, 0) -> bif;
simulate_behaviour(os, putenv, 2) -> bif;
%% Module io
simulate_behaviour(io, printable_range, 0) -> bif;
%% Module slave
%% It is treated as BIF because it includes a registered name in the command
%% that creates a new node and thus cannot work by just overriding
%% the register/1 and unregister/1 commands
simulate_behaviour(slave, start, _A) -> bif;

%simulate_behaviour(whitelist, just_loop, _) -> bif;

%% The rest MFAs are not BIFs if they should be symbolically evaluated.
simulate_behaviour(M, F, A) ->
  Mfa = {M, F, A},
  case no_symbolic_evalution(Mfa) orelse erlang:is_builtin(M, F, A) of
    true  -> bif;
    false -> {ok, Mfa}
  end.

%% ----------------------------------------------------------------------------
%% Internal MFAs without symbolic interpretation.
%% ----------------------------------------------------------------------------

%% Creates a set of MFAs that are defined in cuter_erlang in overriding some
%% Erlang BIFs but have no symbolic interpretation (yet).
internal_mfas_without_symbolic_representation() ->
  gb_sets:from_list([ {cuter_erlang, unsupported_lt, 2} ]).

-spec is_internal_without_symbolic_representation(mfa()) -> boolean().
is_internal_without_symbolic_representation(MFA) ->
  gb_sets:is_member(MFA, internal_mfas_without_symbolic_representation()).

%% ----------------------------------------------------------------------------
%% MFAs that have no meaning in being evaluated symbolically.
%% ----------------------------------------------------------------------------

-spec no_symbolic_evalution(mfa()) -> boolean().
no_symbolic_evalution({os, timestamp, 0}) -> true;
no_symbolic_evalution(_) -> false.

%% ----------------------------------------------------------------------------
%% Whitelisted MFAs that will be treated as BIFs and their code will not be
%% accessed by CutEr.
%% These MFAs need to be exported.
%% ----------------------------------------------------------------------------

%% Checks if an MFA is whitelisted.
-spec is_whitelisted(mfa(), whitelist()) -> boolean().
is_whitelisted({M, F, _A}=MFA, Whitelist) ->
  sets:is_element(MFA, Whitelist) orelse
    sets:is_element({M, F, '*'}, Whitelist) orelse
    sets:is_element({M, '*', '*'}, Whitelist).

%% Generate the whitelist from the data loaded from the file.
-spec parse_whitelist(list()) -> whitelist().
parse_whitelist(LoadedData) -> parse_whitelist(LoadedData, empty_whitelist()).

parse_whitelist([], Acc) ->
  Acc;
parse_whitelist([{M, F, A}=MFA|Rest], Acc) ->
  case is_mfa(M, F, A) orelse is_any_arity(M, F, A) orelse is_any_fun(M, F, A) of
    true  -> parse_whitelist(Rest, sets:add_element(MFA, Acc));
    false -> parse_whitelist(Rest, Acc)
  end;
parse_whitelist([_|Rest], Acc) ->
  parse_whitelist(Rest, Acc).

is_mfa(M, F, A) when is_atom(M), is_atom(F), is_integer(A), A >= 0 -> true;
is_mfa(_, _, _) -> false.

is_any_arity(M, F, A) when is_atom(M), is_atom(F), A =:= '*' -> true;
is_any_arity(_, _, _) -> false.

is_any_fun(M, F, A) when is_atom(M), F =:= '*', A =:= '*' -> true;
is_any_fun(_, _, _) -> false.

-spec get_whitelisted_mfas(whitelist()) -> [mfa()].
get_whitelisted_mfas(Whitelist) -> sets:to_list(Whitelist).

%% Create an empty set of whitelisted MFAs.
-spec empty_whitelist() -> whitelist().
empty_whitelist() -> sets:new().

%% The list of modules that contain funs that override bifs.
-spec overriding_modules() -> [cuter_erlang, ...].
overriding_modules() -> [cuter_erlang].
