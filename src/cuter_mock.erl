%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_mock).

-export([is_whitelisted/2, parse_whitelist/1,
         get_whitelisted_mfas/1, empty_whitelist/0, overriding_modules/0,
         no_symbolic_evalution/1, is_internal_without_symbolic_representation/1,
         maybe_override_mfa/1, is_bif/1]).

-export_type([whitelist/0]).

-include("include/cuter_macros.hrl").

-type entry() :: {module(), atom(), arity()} | {module(), atom(), '*'} | {module(), '*', '*'}.
-type whitelist() :: sets:set(entry()).


%% Optionally replaces one MFA with another.
%% The intention is to override the execution of certain MFAs,
%% and execute our replacements that simulate their behaviour
%% in a way that is beneficial to CutEr, but still sound.
%% All the overrides will be in the cuter_erlang module.
-spec maybe_override_mfa(mfa()) -> mfa().
maybe_override_mfa(Mfa) ->
  case override_mfa(Mfa) of
    {ok, NewMfa} -> NewMfa;
    false -> Mfa
  end.

-spec override_mfa(mfa()) -> {ok, mfa()} | false.
%% Module erlang.
override_mfa({erlang, abs, 1}) -> {ok, {cuter_erlang, abs, 1}};
override_mfa({erlang, 'and', 2}) -> {ok, {cuter_erlang, 'and', 2}};
override_mfa({erlang, 'andalso', 2}) -> {ok, {cuter_erlang, 'andalso', 2}};
override_mfa({erlang, atom_to_list, 1}) -> {ok, {cuter_erlang, atom_to_list, 1}};
override_mfa({erlang, 'div', 2}) -> {ok, {cuter_erlang, 'div', 2}};
override_mfa({erlang, element, 2}) -> {ok, {cuter_erlang, element, 2}};
override_mfa({erlang, float, 1}) -> {ok, {cuter_erlang, float, 1}};
override_mfa({erlang, hd, 1}) -> {ok, {cuter_erlang, hd, 1}};
override_mfa({erlang, integer_to_list, 1}) -> {ok, {cuter_erlang, integer_to_list, 1}};
override_mfa({erlang, list_to_integer, 1}) -> {ok, {cuter_erlang, list_to_integer, 1}};
override_mfa({erlang, length, 1}) -> {ok, {cuter_erlang, length, 1}};
override_mfa({erlang, list_to_tuple, 1}) -> {ok, {cuter_erlang, list_to_tuple, 1}};
override_mfa({erlang, make_tuple, 2}) -> {ok, {cuter_erlang, make_tuple, 2}};
override_mfa({erlang, max, 2}) -> {ok, {cuter_erlang, max, 2}};
override_mfa({erlang, min, 2}) -> {ok, {cuter_erlang, min, 2}};
override_mfa({erlang, 'not', 1}) -> {ok, {cuter_erlang, 'not', 1}};
override_mfa({erlang, 'rem', 2}) -> {ok, {cuter_erlang, 'rem', 2}};
override_mfa({erlang, 'or', 2}) -> {ok, {cuter_erlang, 'or', 2}};
override_mfa({erlang, 'orelse', 2}) -> {ok, {cuter_erlang, 'orelse', 2}};
override_mfa({erlang, setelement, 3}) -> {ok, {cuter_erlang, setelement, 3}};
override_mfa({erlang, tl, 1}) -> {ok, {cuter_erlang, tl, 1}};
override_mfa({erlang, tuple_size, 1}) -> {ok, {cuter_erlang, tuple_size, 1}};
override_mfa({erlang, tuple_to_list, 1}) -> {ok, {cuter_erlang, tuple_to_list, 1}};
override_mfa({erlang, 'xor', 2}) -> {ok, {cuter_erlang, 'xor', 2}};
override_mfa({erlang, '++', 2}) -> {ok, {cuter_erlang, '++', 2}};
override_mfa({erlang, '--', 2}) -> {ok, {cuter_erlang, '--', 2}};
override_mfa({erlang, '=:=', 2}) -> {ok, {cuter_erlang, '=:=', 2}};
override_mfa({erlang, '=/=', 2}) -> {ok, {cuter_erlang, '=/=', 2}};
override_mfa({erlang, '==', 2}) -> {ok, {cuter_erlang, '==', 2}};
override_mfa({erlang, '/=', 2}) -> {ok, {cuter_erlang, '/=', 2}};
override_mfa({erlang, '<', 2}) -> {ok, {cuter_erlang, '<', 2}};
override_mfa({erlang, '=<', 2}) -> {ok, {cuter_erlang, '=<', 2}};
override_mfa({erlang, '>', 2}) -> {ok, {cuter_erlang, '>', 2}};
override_mfa({erlang, '>=', 2}) -> {ok, {cuter_erlang, '>=', 2}};
override_mfa({erlang, '+', 2}) -> {ok, {cuter_erlang, '+', 2}};
override_mfa({erlang, '-', 2}) -> {ok, {cuter_erlang, '-', 2}};
override_mfa({erlang, '*', 2}) -> {ok, {cuter_erlang, '*', 2}};
override_mfa({erlang, '/', 2}) -> {ok, {cuter_erlang, '/', 2}};
override_mfa({erlang, is_binary, 1}) -> {ok, {cuter_erlang, is_binary, 1}};
override_mfa({erlang, bit_size, 1}) -> {ok, {cuter_erlang, bit_size, 1}};
override_mfa({erlang, byte_size, 1}) -> {ok, {cuter_erlang, byte_size, 1}};
override_mfa({erlang, 'bsl', 2}) -> {ok, {cuter_erlang, 'bsl', 2}};
override_mfa({erlang, 'bsr', 2}) -> {ok, {cuter_erlang, 'bsr', 2}};
override_mfa({erlang, 'bnot', 1}) -> {ok, {cuter_erlang, 'bnot', 1}};
override_mfa({erlang, trunc, 1}) -> {ok, {cuter_erlang, trunc, 1}};
%% Module lists.
override_mfa({lists, member, 2}) -> {ok, {cuter_erlang, member,  2}};
override_mfa({lists, reverse, 2}) -> {ok, {cuter_erlang, reverse, 2}};
override_mfa({lists, keyfind, 3}) -> {ok, {cuter_erlang, keyfind, 3}};
%% Module math.
override_mfa({math, pi, 0}) -> {ok, {math, pi, 0}};
%% The rest MFAs are not overriden.
override_mfa(_Mfa) -> false.

%% Returns true if the MFA should be considered a BIF in the context of a
%% concolic execution.
-spec is_bif(mfa()) -> boolean().
%% Module erlang.
is_bif({erlang, _F, _A}) -> true;
%% Module cuter_erlang.
is_bif({cuter_erlang, _F, _A}=Mfa) ->
  gb_sets:is_member(Mfa, cuter_log:supported_mfas())
  orelse is_internal_without_symbolic_representation(Mfa);
%% Module beam_asm.
is_bif({beam_asm, _F, _A}) -> true;
%% Module beam_lib.
is_bif({beam_lib, _F, _A}) -> true;
%% Module binary.
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
%% Module epp.
is_bif({epp, _F, _A}) -> true;
%% Module crypto.
is_bif({crypto, md5, 1}) -> true;
is_bif({crypto, info_lib, 0}) -> true;
is_bif({crypto, hmac_init, 2}) -> true;
is_bif({crypto, hmac_final, 1}) -> true;
is_bif({crypto, hmac_final_n, 2}) -> true;
is_bif({crypto, rand_bytes, 1}) -> true;
is_bif({crypto, strong_rand_bytes_nif, 1}) -> true;
is_bif({crypto, rand_bytes, 3}) -> true;
is_bif({crypto, rand_seed_nif, 1}) -> true;
is_bif({crypto, md5_init, 0}) -> true;
is_bif({crypto, md5_update, 2}) -> true;
is_bif({crypto, md5_final, 1}) -> true;
is_bif({crypto, md4, 1}) -> true;
is_bif({crypto, md4_init, 0}) -> true;
is_bif({crypto, md4_update, 2}) -> true;
is_bif({crypto, md4_final, 1}) -> true;
is_bif({crypto, ripemd160, 1}) -> true;
is_bif({crypto, ripemd160_init, 0}) -> true;
is_bif({crypto, ripemd160_update, 2}) -> true;
is_bif({crypto, ripemd160_final, 1}) -> true;
is_bif({crypto, sha, 1}) -> true;
is_bif({crypto, sha_init, 0}) -> true;
is_bif({crypto, sha_update, 2}) -> true;
is_bif({crypto, sha_final, 1}) -> true;
is_bif({crypto, sha224_nif, 1}) -> true;
is_bif({crypto, sha224_init_nif, 0}) -> true;
is_bif({crypto, sha224_update_nif, 2}) -> true;
is_bif({crypto, sha224_final_nif, 1}) -> true;
is_bif({crypto, sha256_nif, 1}) -> true;
is_bif({crypto, sha256_init_nif, 0}) -> true;
is_bif({crypto, sha256_update_nif, 2}) -> true;
is_bif({crypto, sha256_final_nif, 1}) -> true;
is_bif({crypto, sha384_nif, 1}) -> true;
is_bif({crypto, sha384_init_nif, 0}) -> true;
is_bif({crypto, sha384_update_nif, 2}) -> true;
is_bif({crypto, sha384_final_nif, 1}) -> true;
is_bif({crypto, sha512_nif, 1}) -> true;
is_bif({crypto, sha512_init_nif, 0}) -> true;
is_bif({crypto, sha512_update_nif, 2}) -> true;
is_bif({crypto, sha512_final_nif, 1}) -> true;
is_bif({crypto, do_hmac_update, 2}) -> true;
is_bif({crypto, md5_mac_n, 3}) -> true;
is_bif({crypto, sha_mac_n, 3}) -> true;
is_bif({crypto, sha224_mac_nif, 3}) -> true;
is_bif({crypto, sha256_mac_nif, 3}) -> true;
is_bif({crypto, sha384_mac_nif, 3}) -> true;
is_bif({crypto, sha512_mac_nif, 3}) -> true;
is_bif({crypto, des_ecb_crypt, 3}) -> true;
is_bif({crypto, des_ede3_cbc_crypt, 6}) -> true;
is_bif({crypto, des_ede3_cfb_crypt_nif, 6}) -> true;
is_bif({crypto, bf_ecb_crypt, 3}) -> true;
is_bif({crypto, bf_cbc_crypt, 4}) -> true;
is_bif({crypto, bf_cfb64_crypt, 4}) -> true;
is_bif({crypto, blowfish_ofb64_encrypt, 3}) -> true;
is_bif({crypto, aes_cfb_8_crypt, 4}) -> true;
is_bif({crypto, aes_cfb_128_crypt, 4}) -> true;
is_bif({crypto, des_cbc_crypt, 4}) -> true;
is_bif({crypto, des_cfb_crypt, 4}) -> true;
is_bif({crypto, aes_cbc_crypt, 4}) -> true;
is_bif({crypto, aes_ige_crypt_nif, 4}) -> true;
is_bif({crypto, aes_ctr_encrypt, 3}) -> true;
is_bif({crypto, aes_ctr_decrypt, 3}) -> true;
is_bif({crypto, aes_ctr_stream_encrypt, 2}) -> true;
is_bif({crypto, aes_ctr_stream_decrypt, 2}) -> true;
is_bif({crypto, rc4_encrypt, 2}) -> true;
is_bif({crypto, rc4_set_key, 1}) -> true;
is_bif({crypto, rc4_encrypt_with_state, 2}) -> true;
is_bif({crypto, rc2_cbc_crypt, 4}) -> true;
is_bif({crypto, srp_host_secret_nif, 5}) -> true;
is_bif({crypto, rsa_sign_nif, 3}) -> true;
is_bif({crypto, dss_sign_nif, 3}) -> true;
is_bif({crypto, ecdsa_sign_nif, 4}) -> true;
is_bif({crypto, dss_verify_nif, 4}) -> true;
is_bif({crypto, rsa_verify_nif, 4}) -> true;
is_bif({crypto, ecdsa_verify_nif, 5}) -> true;
is_bif({crypto, dh_check, 1}) -> true;
is_bif({crypto, dh_generate_key_nif, 3}) -> true;
is_bif({crypto, dh_compute_key_nif, 3}) -> true;
is_bif({crypto, ec_key_generate, 1}) -> true;
is_bif({crypto, ecdh_compute_key_nif, 3}) -> true;
is_bif({crypto, do_exor, 2}) -> true;
is_bif({crypto, algorithms, 0}) -> true;
is_bif({crypto, rsa_public_crypt, 4}) -> true;
is_bif({crypto, rsa_private_crypt, 4}) -> true;
is_bif({crypto, strong_rand_mpint_nif, 3}) -> true;
is_bif({crypto, mod_exp_nif, 5}) -> true;
is_bif({crypto, rand_uniform, 2}) -> true;  %% Depends on NIF crypto:rand_uniform_nif/2
%% Module ets.
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
%% Module file.
is_bif({file, native_name_encoding, 0}) -> true;
%% Module lists.
is_bif({lists, keymember, 3}) -> true;
is_bif({lists, keysearch, 3}) -> true;
%% Module math.
is_bif({math, _F, _A}) -> true;
%% Module net_kernel.
is_bif({net_kernel, dflag_unicode_io, 1}) -> true;
%% Module os.
is_bif({os, getenv, 0}) -> true;
is_bif({os, getenv, 1}) -> true;
is_bif({os, getpid, 0}) -> true;
is_bif({os, putenv, 2}) -> true;
%% Module io.
is_bif({io, printable_range, 0}) -> true;
%% Module slave.
%% It is treated as BIF because it includes a registered name in the command
%% that creates a new node and thus cannot work by just overriding
%% the register/1 and unregister/1 commands
is_bif({slave, start, _A}) -> true;
%% The rest MFAs are not BIFs if they should be symbolically evaluated.
is_bif({M, F, A}=Mfa) ->
  no_symbolic_evalution(Mfa) orelse erlang:is_builtin(M, F, A).

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
