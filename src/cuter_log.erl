%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_log).

-include("include/cuter_macros.hrl").

-export([
    close_file/1
  , locate_reversible/1
  , log_equal/5
  , log_guard/4
  , log_list/4
  , log_make_tuple/3
  , log_make_bitstring/4
  , log_mfa/4
  , log_lambda/4
  , log_message_consumed/3
  , log_message_received/3
  , log_message_sent/3
  , log_spawn/3
  , log_spawned/3
  , log_spec/2
  , log_symb_params/2
  , log_tuple/5
  , log_unfold_symbolic/4
  , next_entry/1
  , open_file/2
  , path_vertex/1
  , reduce_constraint_counter/0
  , write_data/2
  , log_empty_bitstring/2
  , log_nonempty_bitstring/4
  , log_concat_segments/4
  , log_bitmatch_const_true/6
  , log_bitmatch_const_false/5
  , log_bitmatch_var_true/6
  , log_bitmatch_var_false/4
  , log_bitsize_not_equal/3
  , log_bitsize_equal/3
  , log_not_lambda_with_arity/4
  , log_fresh_lambda/3
  , log_evaluated_closure/4
]).
-export([supported_mfas/0]).

-export_type([opcode/0]).

-type mode()   :: read | write.

-type opcode() :: 'OP_PARAMS' | 'OP_SPEC'
                %% Lambda application.
                | 'OP_LAMBDA' | 'OP_EVALUATED_CLOSURE'
                | 'OP_FRESH_LAMBDA_WITH_ARITY' | 'OP_NOT_LAMBDA_WITH_ARITY'
                %% Constraints.
                | 'OP_GUARD_TRUE' | 'OP_GUARD_FALSE'
                | 'OP_MATCH_EQUAL_TRUE' | 'OP_MATCH_EQUAL_FALSE'
                | 'OP_TUPLE_SZ' | 'OP_TUPLE_NOT_SZ' | 'OP_TUPLE_NOT_TPL'
                | 'OP_LIST_NON_EMPTY' | 'OP_LIST_EMPTY' | 'OP_LIST_NOT_LST'
                | 'OP_BITMATCH_CONST_TRUE' | 'OP_BITMATCH_CONST_FALSE'
                | 'OP_BITMATCH_VAR_TRUE' | 'OP_BITMATCH_VAR_FALSE'
                %% Information used for syncing & merging the traces of many processes.
                | 'OP_SPAWN' | 'OP_SPAWNED'
                | 'OP_MSG_SEND' | 'OP_MSG_RECEIVE' | 'OP_MSG_CONSUME'
                %% Necessary operations for the evaluation of Core Erlang.
                | 'OP_UNFOLD_TUPLE' | 'OP_UNFOLD_LIST'
                %% Bogus operation (operations interpreted as the identity function).
                | 'OP_BOGUS'
                %% Type conversions.
                | 'OP_FLOAT' | 'OP_LIST_TO_TUPLE' | 'OP_TUPLE_TO_LIST'
                %% Query types.
                | 'OP_IS_INTEGER' | 'OP_IS_ATOM' | 'OP_IS_FLOAT'
                | 'OP_IS_LIST' | 'OP_IS_TUPLE' | 'OP_IS_BOOLEAN'
                | 'OP_IS_NUMBER' | 'OP_IS_BITSTRING'
                | 'OP_IS_FUN' | 'OP_IS_FUN_WITH_ARITY'
                %% Arithmetic operations.
                | 'OP_PLUS' | 'OP_MINUS' | 'OP_TIMES' | 'OP_RDIV'
                | 'OP_IDIV_NAT' | 'OP_REM_NAT' | 'OP_UNARY'
                | 'OP_POW' | 'OP_TRUNC'
                %% Operations on atoms.
                | 'OP_ATOM_NIL' | 'OP_ATOM_HEAD' | 'OP_ATOM_TAIL'
                %% Operations on lists.
                | 'OP_HD' | 'OP_TL' | 'OP_CONS'
                %% Operations on tuples.
                | 'OP_TCONS'
                %% Comparisons.
                | 'OP_EQUAL' | 'OP_UNEQUAL' | 'OP_LT_INT' | 'OP_LT_FLOAT'
                %% Make binaries & bitstrings.
                | 'OP_MAKE_BITSTR' | 'OP_EMPTY_BITSTR'
                | 'OP_NONEMPTY_BITSTR'| 'OP_CONCAT_SEGS'
                %% Bitwise operations.
                | 'OP_BAND' | 'OP_BXOR' | 'OP_BOR'
                .

%% ============================================================================
%% Public API.
%% ============================================================================

%% Returns a set of all the MFAs that are supported for symbolic evaluation.
-spec supported_mfas() -> gb_sets:set(opcode()).
supported_mfas() ->
  gb_sets:from_list(dict:fetch_keys(opcode_mappings())).

%% Opens a file for logging or reading terms.
-spec open_file(file:name(), mode()) -> {ok, file:io_device()}.
open_file(Fd, read) ->
  file:open(Fd, [read, raw, binary, compressed, {read_ahead, 262144}]);
open_file(Fd, write) ->
  file:open(Fd, [write, raw, binary, compressed, {delayed_write, 262144, 2000}]).

%% Closes a file.
-spec close_file(file:io_device()) -> ok.
close_file(Fd) ->
  ok = file:close(Fd).

%% ------------------------------------------------------------------
%% Log symbolic MFA & lambda applications
%% ------------------------------------------------------------------

-spec log_mfa(file:io_device(), mfa(), [any()], cuter_symbolic:symbolic()) -> ok.
log_mfa(Fd, MFA, SAs, X) ->
  %% SAs has at least one symbolic argument 
  %% as ensured by cuter_sumbolic:evaluate_mfa/4
  log(Fd, mfa2op(MFA), ?EMPTY_TAG_ID, [X | SAs]).

-spec log_lambda(file:io_device(), cuter_symbolic:symbolic(), [any()], cuter_symbolic:symbolic()) -> ok.
log_lambda(Fd, FunS, ArgsS, ResultS) ->
  log(Fd, 'OP_LAMBDA', ?EMPTY_TAG_ID, [ResultS, FunS | ArgsS]).

-spec log_fresh_lambda(file:io_device(), cuter_symbolic:symbolic(), arity()) -> ok.
log_fresh_lambda(Fd, FunS, Arity) ->
  log(Fd, 'OP_FRESH_LAMBDA_WITH_ARITY', ?EMPTY_TAG_ID, [FunS, Arity]).

-spec log_evaluated_closure(file:io_device(), cuter_symbolic:symbolic(), [any()], cuter_symbolic:symbolic()) -> ok.
log_evaluated_closure(Fd, LambdaS, ArgsS, ResultS) ->
  log(Fd, 'OP_EVALUATED_CLOSURE', ?EMPTY_TAG_ID, [ResultS, LambdaS | ArgsS]).

%% ------------------------------------------------------------------
%% Log Entry Point MFA's parameters & spec
%% ------------------------------------------------------------------

-spec log_symb_params(file:io_device(), [cuter_symbolic:symbolic()]) -> ok.
log_symb_params(_Fd, []) -> ok;
log_symb_params(Fd, Ps)  -> log(Fd, 'OP_PARAMS', ?EMPTY_TAG_ID, Ps).

-spec log_spec(file:io_device(), cuter_types:erl_spec()) -> ok.
log_spec(Fd, Spec)  -> log(Fd, 'OP_SPEC', ?EMPTY_TAG_ID, [Spec]).

%% ------------------------------------------------------------------
%% Log process spawns
%% ------------------------------------------------------------------

-spec log_spawn(file:io_device(), pid(), reference()) -> ok.
log_spawn(Fd, Child, Ref) ->
  log(Fd, 'OP_SPAWN', ?EMPTY_TAG_ID, [node(Child), Child, Ref]).

-spec log_spawned(file:io_device(), pid(), reference()) -> ok.
log_spawned(Fd, Parent, Ref) ->
  log(Fd, 'OP_SPAWNED', ?EMPTY_TAG_ID, [node(Parent), Parent, Ref]).

%% ------------------------------------------------------------------
%% Log message passing
%% ------------------------------------------------------------------

-spec log_message_sent(file:io_device(), pid(), reference()) -> ok.
log_message_sent(Fd, Dest, Ref) ->
  log(Fd, 'OP_MSG_SEND', ?EMPTY_TAG_ID, [node(Dest), Dest, Ref]).

-spec log_message_received(file:io_device(), pid(), reference()) -> ok.
log_message_received(Fd, From, Ref) ->
  log(Fd, 'OP_MSG_RECEIVE', ?EMPTY_TAG_ID, [node(From), From, Ref]).

-spec log_message_consumed(file:io_device(), pid(), reference()) -> ok.
log_message_consumed(Fd, From, Ref) ->
  log(Fd, 'OP_MSG_CONSUME', ?EMPTY_TAG_ID, [node(From), From, Ref]).

%% ------------------------------------------------------------------
%% Log the unfolding of a symbolic variable that represents 
%% a list or a tuple of values
%% ------------------------------------------------------------------

-spec log_unfold_symbolic(file:io_device(), (break_tuple | break_list), cuter_symbolic:symbolic(), [cuter_symbolic:symbolic()]) -> ok.
log_unfold_symbolic(Fd, break_tuple, Sv, Vs) ->
  log(Fd, 'OP_UNFOLD_TUPLE', ?EMPTY_TAG_ID, [Sv | Vs]);
log_unfold_symbolic(Fd, break_list, Sv, Vs) ->
  log(Fd, 'OP_UNFOLD_LIST', ?EMPTY_TAG_ID, [Sv | Vs]).

%% ------------------------------------------------------------------
%% Logs the creation of a tuple.
%% ------------------------------------------------------------------

-spec log_make_tuple(file:io_device(), cuter_symbolic:symbolic(), [any()]) -> ok.
log_make_tuple(Fd, Sv, Xs) ->
  log(Fd, 'OP_TCONS', ?EMPTY_TAG_ID, [Sv | Xs]).

%% ------------------------------------------------------------------
%% Log binary / bitstring creation.
%% ------------------------------------------------------------------

-spec log_make_bitstring(file:io_device(), cuter_symbolic:symbolic(), any(), integer()) -> ok.
log_make_bitstring(Fd, Sv, V, Sz) ->
  log(Fd, 'OP_MAKE_BITSTR', ?EMPTY_TAG_ID, [Sv, V, Sz]).

%% TODO Use a proper tag.
-spec log_empty_bitstring(file:io_device(), cuter_symbolic:symbolic()) -> ok.
log_empty_bitstring(Fd, Sv) ->
  log(Fd, 'OP_EMPTY_BITSTR', ?EMPTY_TAG_ID, [Sv]).

%% TODO Use a proper tag.
-spec log_nonempty_bitstring(file:io_device(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic()) -> ok.
log_nonempty_bitstring(Fd, H, T, Sv) ->
  log(Fd, 'OP_NONEMPTY_BITSTR', ?EMPTY_TAG_ID, [H, T, Sv]).

%% TODO Use a proper tag.
-spec log_concat_segments(file:io_device(), cuter_symbolic:symbolic(), [any()], any()) -> ok.
log_concat_segments(Fd, Sv1, Bits, Sv) ->
  log(Fd, 'OP_CONCAT_SEGS', ?EMPTY_TAG_ID, [Sv1, Sv | Bits]).

-spec log_bitmatch_const_true(file:io_device(), any(), integer(), any(), cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_const_true(Fd, Cnst, Size, Sv, Sv1, Tag) ->
  log(Fd, 'OP_BITMATCH_CONST_TRUE', cuter_cerl:id_of_tag(Tag), [Sv1, Cnst, Size, Sv]).

-spec log_bitmatch_const_false(file:io_device(), any(), integer(), any(), cuter_cerl:tag()) -> ok.
log_bitmatch_const_false(Fd, Cnst, Size, Sv, Tag) ->
  log(Fd, 'OP_BITMATCH_CONST_FALSE', cuter_cerl:id_of_tag(Tag), [Cnst, Size, Sv]).

-spec log_bitmatch_var_true(file:io_device(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic(), integer(), 
                            cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_var_true(Fd, Sv1, Sv2, Size, Sv, Tag) ->
  log(Fd, 'OP_BITMATCH_VAR_TRUE', cuter_cerl:id_of_tag(Tag), [Sv1, Sv2, Size, Sv]).

-spec log_bitmatch_var_false(file:io_device(), integer(), cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_var_false(Fd, Size, Sv, Tag) ->
  log(Fd, 'OP_BITMATCH_VAR_FALSE', cuter_cerl:id_of_tag(Tag), [Size, Sv]).

-spec log_bitsize_equal(file:io_device(), cuter_symbolic:symbolic(), integer()) -> ok.
log_bitsize_equal(Fd, Sv, Cv) ->
  log_equal(Fd, true, Sv, Cv, cuter_cerl:empty_tag()).

-spec log_bitsize_not_equal(file:io_device(), cuter_symbolic:symbolic(), integer()) -> ok.
log_bitsize_not_equal(Fd, Sv, Cv) ->
  log_equal(Fd, false, Sv, Cv, cuter_cerl:empty_tag()).

%% ------------------------------------------------------------------
%% Log Constraints
%% ------------------------------------------------------------------

-spec log_guard(file:io_device(), boolean(), any(), cuter_cerl:tag()) -> ok.
%% True guard
log_guard(Fd, true, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_GUARD_TRUE', cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% False guard
log_guard(Fd, false, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_GUARD_FALSE', cuter_cerl:id_of_tag(Tag), [Sv])
  end.

-spec log_equal(file:io_device(), boolean(), any(), any(), cuter_cerl:tag()) -> ok.
%% Match equal
log_equal(Fd, true, Sv1, Sv2, Tag) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, 'OP_MATCH_EQUAL_TRUE', cuter_cerl:id_of_tag(Tag), [Sv1, Sv2])
  end;
%% Match not equal
log_equal(Fd, false, Sv1, Sv2, Tag) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, 'OP_MATCH_EQUAL_FALSE', cuter_cerl:id_of_tag(Tag), [Sv1, Sv2])
  end.

-spec log_tuple(file:io_device(), (sz | not_sz | not_tpl), any(), integer(), cuter_cerl:tag()) -> ok.
%% Tuple of size N
log_tuple(Fd, sz, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_TUPLE_SZ', cuter_cerl:id_of_tag(Tag), [Sv, N])
  end;
%% Tuple of not size N
log_tuple(Fd, not_sz, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_TUPLE_NOT_SZ', cuter_cerl:id_of_tag(Tag), [Sv, N])
  end;
%% Not a tuple
log_tuple(Fd, not_tpl, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_TUPLE_NOT_TPL', cuter_cerl:id_of_tag(Tag), [Sv, N])
  end.

-spec log_list(file:io_device(), (nonempty | empty | not_lst), any(), cuter_cerl:tag()) -> ok.
%% Non-empty list
log_list(Fd, nonempty, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_LIST_NON_EMPTY', cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% Empty list
log_list(Fd, empty, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_LIST_EMPTY', cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% Not a list
log_list(Fd, not_lst, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, 'OP_LIST_NOT_LST', cuter_cerl:id_of_tag(Tag), [Sv])
  end.

%% Not a lambda with arity.
-spec log_not_lambda_with_arity(file:io_device(), cuter_symbolic:symbolic(), arity(), cuter_cerl:tag()) -> ok.
log_not_lambda_with_arity(Fd, FunS, Arity, Tag) ->
  log(Fd, 'OP_NOT_LAMBDA_WITH_ARITY', cuter_cerl:id_of_tag(Tag), [FunS, Arity]).

%% ------------------------------------------------------------------
%% Logging Function
%% ------------------------------------------------------------------

%% Log data to a file
-spec log(file:io_device(), opcode(), cuter_cerl:tagID(), [any()]) -> ok.
-ifdef(LOGGING_FLAG).
log(Fd, OpCode, TagID, Data) ->
  case get(?DEPTH_PREFIX) of
    undefined -> throw(depth_undefined_in_pdict);
    0 -> ok;
    N when is_integer(N), N > 0 ->
      IsConstraint = is_constraint(OpCode),
      try cuter_serial:to_log_entry(OpCode, Data, IsConstraint, TagID) of
        Jdata ->
          write_data(Fd, Jdata)
      catch
        throw:{unsupported_term, _} -> ok
      end
  end.
-else.
log(_, _, _) -> ok.
-endif.

%% Get the JSON Opcode of an MFA.
-spec mfa2op(mfa()) -> opcode().
mfa2op(MFA) ->
  dict:fetch(MFA, opcode_mappings()).

opcode_mappings() ->
  dict:from_list([ %% Simulated built-in operations
                   { {cuter_erlang, atom_to_list_bogus, 1}, 'OP_BOGUS'         }
                 , { {cuter_erlang, is_atom_nil,        1}, 'OP_ATOM_NIL'      }
                 , { {cuter_erlang, safe_atom_head,     1}, 'OP_ATOM_HEAD'     }
                 , { {cuter_erlang, safe_atom_tail,     1}, 'OP_ATOM_TAIL'     }
                 , { {cuter_erlang, safe_pos_div,       2}, 'OP_IDIV_NAT'      }
                 , { {cuter_erlang, safe_pos_rem,       2}, 'OP_REM_NAT'       }
                 , { {cuter_erlang, lt_int,             2}, 'OP_LT_INT'        }
                 , { {cuter_erlang, lt_float,           2}, 'OP_LT_FLOAT'      }
                 , { {cuter_erlang, safe_plus,          2}, 'OP_PLUS'          }
                 , { {cuter_erlang, safe_minus,         2}, 'OP_MINUS'         }
                 , { {cuter_erlang, safe_times,         2}, 'OP_TIMES'         }
                 , { {cuter_erlang, safe_rdiv,          2}, 'OP_RDIV'          }
                 , { {cuter_erlang, safe_float,         1}, 'OP_FLOAT'         }
                 , { {cuter_erlang, safe_list_to_tuple, 1}, 'OP_LIST_TO_TUPLE' }
                 , { {cuter_erlang, safe_tuple_to_list, 1}, 'OP_TUPLE_TO_LIST' }
                 , { {cuter_erlang, safe_hd,            1}, 'OP_HD'            }
                 , { {cuter_erlang, safe_tl,            1}, 'OP_TL'            }
                 , { {cuter_erlang, basic_eq,           2}, 'OP_EQUAL'         }
                 , { {cuter_erlang, safe_trunc,         1}, 'OP_TRUNC'         }
                 , { {bogus_erlang, cons,               2}, 'OP_CONS'          }
                   %% Actual erlang BIFs
                 , { {erlang, is_integer,    1}, 'OP_IS_INTEGER'        }
                 , { {erlang, is_atom,       1}, 'OP_IS_ATOM'           }
                 , { {erlang, is_boolean,    1}, 'OP_IS_BOOLEAN'        }
                 , { {erlang, is_float,      1}, 'OP_IS_FLOAT'          }
                 , { {erlang, is_list,       1}, 'OP_IS_LIST'           }
                 , { {erlang, is_tuple,      1}, 'OP_IS_TUPLE'          }
                 , { {erlang, is_number,     1}, 'OP_IS_NUMBER'         }
                 , { {erlang, '-',           1}, 'OP_UNARY'             }
                 , { {math, pow,             2}, 'OP_POW'               }
                 , { {erlang, is_bitstring,  1}, 'OP_IS_BITSTRING'      }
                 , { {erlang, is_function,   1}, 'OP_IS_FUN'            }
                 , { {erlang, is_function,   2}, 'OP_IS_FUN_WITH_ARITY' }
                 , { {erlang, 'band',        2}, 'OP_BAND'              }
                 , { {erlang, 'bor',         2}, 'OP_BOR'               }
                 , { {erlang, 'bxor',        2}, 'OP_BXOR'              }
                 ]).

%% Checks if a log entry is a constraint.
-spec is_constraint(opcode()) -> boolean().
is_constraint(OpCode) ->
  gb_sets:is_element(OpCode, constraint_true_opcodes())
    orelse gb_sets:is_element(OpCode, constraint_false_opcodes()).

constraint_true_opcodes() ->
  gb_sets:from_list(['OP_LAMBDA'
                   , 'OP_GUARD_TRUE'
                   , 'OP_MATCH_EQUAL_TRUE'
                   , 'OP_TUPLE_SZ'
                   , 'OP_LIST_NON_EMPTY'
                   , 'OP_NONEMPTY_BITSTR'
                   , 'OP_BITMATCH_CONST_TRUE'
                   , 'OP_BITMATCH_VAR_TRUE'
                   ]).

constraint_false_opcodes() ->
  gb_sets:from_list(['OP_GUARD_FALSE'
                   , 'OP_MATCH_EQUAL_FALSE'
                   , 'OP_TUPLE_NOT_SZ'
                   , 'OP_TUPLE_NOT_TPL'
                   , 'OP_LIST_EMPTY'
                   , 'OP_LIST_NOT_LST'
                   , 'OP_EMPTY_BITSTR'
                   , 'OP_BITMATCH_CONST_FALSE'
                   , 'OP_BITMATCH_VAR_FALSE'
                   , 'OP_NOT_LAMBDA_WITH_ARITY'
                   ]).

%% Reduce the counter that controls the logging of constraints.
-spec reduce_constraint_counter() -> ok.
reduce_constraint_counter() ->
  case get(?DEPTH_PREFIX) of
    undefined -> throw(depth_undefined_in_pdict);
    0 -> ok;
    N when N > 0 -> _ = put(?DEPTH_PREFIX, N - 1), ok
  end.

%% ------------------------------------------------------------------
%% Read / Write Data
%% ------------------------------------------------------------------

-spec write_data(file:io_device(), binary()) -> ok.
write_data(Fd, Data) when is_binary(Data) ->
  Sz = erlang:byte_size(Data),
  ok = file:write(Fd, [i32_to_list(Sz), Data]).

%% Encode a 32-bit integer to its corresponding sequence of four bytes
-spec i32_to_list(non_neg_integer()) -> [byte(), ...].
i32_to_list(Int) when is_integer(Int) ->
  [(Int bsr 24) band 255,
   (Int bsr 16) band 255,
   (Int bsr  8) band 255,
    Int band 255].

%% Decode a 4-byte binary to the corresponding 32-bit number
-spec bin_to_i32(binary()) -> non_neg_integer().
bin_to_i32(B) when is_binary(B) ->
  [X1, X2, X3, X4] = erlang:binary_to_list(B, 1, 4),
  (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

%% Generate the path vertex from a trace file
-spec path_vertex(file:name()) -> cuter_analyzer:path_vertex().
path_vertex(File) ->
  {ok, Fd} = open_file(File, read),
  generate_vertex(Fd, []).

-spec generate_vertex(file:io_device(), cuter_analyzer:path_vertex()) -> cuter_analyzer:path_vertex().
generate_vertex(Fd, Acc) ->
  case next_entry(Fd) of
    eof -> lists:reverse(Acc);
    Data ->
      {OpCode, _Args, IsConstraint, _TagId} = cuter_serial:from_log_entry(Data),
      case IsConstraint of
        false -> generate_vertex(Fd, Acc);
        true -> generate_vertex(Fd, [map_constraint_to_repr(OpCode)|Acc])
      end
  end.

map_constraint_to_repr(OpCode) ->
  case gb_sets:is_element(OpCode, constraint_true_opcodes()) of
    true  -> ?CONSTRAINT_TRUE_REPR;
    false -> ?CONSTRAINT_FALSE_REPR
  end.

%% Locate the reversible commands with their tag IDs in a trace file.
-spec locate_reversible(file:name()) -> cuter_analyzer:reversible_with_tags().
locate_reversible(File) ->
  {ok, Fd} = open_file(File, read),
  locate_reversible(Fd, 0, []).

-spec locate_reversible(file:io_device(), non_neg_integer(), cuter_analyzer:reversible_with_tags()) -> cuter_analyzer:reversible_with_tags().
locate_reversible(Fd, N, Acc) ->
  N1 = N + 1,
  case next_entry(Fd) of
    eof -> lists:reverse(Acc);
    Data ->
      {_OpCode, _Args, IsConstraint, TagId} = cuter_serial:from_log_entry(Data),
      case IsConstraint of
        false -> locate_reversible(Fd, N, Acc);
        true  -> locate_reversible(Fd, N1, [{N1, TagId}|Acc])
      end
  end.

-spec next_entry(file:io_device()) -> eof | binary().
next_entry(Fd) ->
  case safe_read(Fd, 4, true) of
    eof ->
      close_file(Fd),
      eof;
    SzBin ->
      Sz = bin_to_i32(SzBin),
      safe_read(Fd, Sz, false)
  end.

-spec safe_read(file:io_device(), integer(), boolean()) -> binary() | eof.
safe_read(Fd, Sz, AllowEOF) ->
  case file:read(Fd, Sz) of
    {ok, Bin} -> Bin;
    eof when AllowEOF -> eof;
    eof -> throw(unexpected_eof);
    {error, Reason} -> throw({file_read_failed, Reason})
  end.
