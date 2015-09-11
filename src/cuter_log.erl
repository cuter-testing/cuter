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
  , log_mfa/5
  , log_message_consumed/3
  , log_message_received/3
  , log_message_sent/3
  , log_spawn/3
  , log_spawned/3
  , log_spec/2
  , log_symb_params/2
  , log_tuple/5
  , log_unfold_symbolic/4
  , next_entry/2
  , open_file/2
  , path_vertex/1
  , reduce_constraint_counter/0
  , write_data/5
  , log_empty_bitstring/2
  , log_nonempty_bitstring/4
  , log_concat_segments/4
  , log_bitmatch_const_true/6
  , log_bitmatch_const_false/5
  , log_bitmatch_var_true/6
  , log_bitmatch_var_false/4
  , log_bitsize_not_equal/3
  , log_bitsize_equal/3
]).

-export_type([opcode/0]).

-type mode()   :: read | write.
-type opcode() :: byte().
-type entry_type() :: ?CONSTRAINT_TRUE | ?CONSTRAINT_FALSE | ?NOT_CONSTRAINT.

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
%% Log a symbolic MFA operation
%% ------------------------------------------------------------------

-spec log_mfa(file:io_device(), mfa(), [any()], cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_mfa(Fd, MFA, SAs, X, Tag) ->
  %% SAs has at least one symbolic argument 
  %% as ensured by cuter_sumbolic:evaluate_mfa/4
  log(Fd, mfa2op(MFA), cuter_cerl:id_of_tag(Tag), [X | SAs]).

%% ------------------------------------------------------------------
%% Log Entry Point MFA's parameters & spec
%% ------------------------------------------------------------------

-spec log_symb_params(file:io_device(), [cuter_symbolic:symbolic()]) -> ok.
log_symb_params(_Fd, []) -> ok;
log_symb_params(Fd, Ps)  -> log(Fd, ?OP_PARAMS, ?EMPTY_TAG_ID, Ps).

-spec log_spec(file:io_device(), cuter_types:erl_spec()) -> ok.
log_spec(Fd, Spec)  -> log(Fd, ?OP_SPEC, ?EMPTY_TAG_ID, [Spec]).

%% ------------------------------------------------------------------
%% Log process spawns
%% ------------------------------------------------------------------

-spec log_spawn(file:io_device(), pid(), reference()) -> ok.
log_spawn(Fd, Child, Ref) ->
  log(Fd, ?OP_SPAWN, ?EMPTY_TAG_ID, [node(Child), Child, Ref]).

-spec log_spawned(file:io_device(), pid(), reference()) -> ok.
log_spawned(Fd, Parent, Ref) ->
  log(Fd, ?OP_SPAWNED, ?EMPTY_TAG_ID, [node(Parent), Parent, Ref]).

%% ------------------------------------------------------------------
%% Log message passing
%% ------------------------------------------------------------------

-spec log_message_sent(file:io_device(), pid(), reference()) -> ok.
log_message_sent(Fd, Dest, Ref) ->
  log(Fd, ?OP_MSG_SEND, ?EMPTY_TAG_ID, [node(Dest), Dest, Ref]).

-spec log_message_received(file:io_device(), pid(), reference()) -> ok.
log_message_received(Fd, From, Ref) ->
  log(Fd, ?OP_MSG_RECEIVE, ?EMPTY_TAG_ID, [node(From), From, Ref]).

-spec log_message_consumed(file:io_device(), pid(), reference()) -> ok.
log_message_consumed(Fd, From, Ref) ->
  log(Fd, ?OP_MSG_CONSUME, ?EMPTY_TAG_ID, [node(From), From, Ref]).

%% ------------------------------------------------------------------
%% Log the unfolding of a symbolic variable that represents 
%% a list or a tuple of values
%% ------------------------------------------------------------------

-spec log_unfold_symbolic(file:io_device(), (break_tuple | break_list), cuter_symbolic:symbolic(), [cuter_symbolic:symbolic()]) -> ok.
log_unfold_symbolic(Fd, break_tuple, Sv, Vs) ->
  log(Fd, ?OP_UNFOLD_TUPLE, ?EMPTY_TAG_ID, [Sv | Vs]);
log_unfold_symbolic(Fd, break_list, Sv, Vs) ->
  log(Fd, ?OP_UNFOLD_LIST, ?EMPTY_TAG_ID, [Sv | Vs]).

%% ------------------------------------------------------------------
%% Logs the creation of a tuple.
%% ------------------------------------------------------------------

-spec log_make_tuple(file:io_device(), cuter_symbolic:symbolic(), [any()]) -> ok.
log_make_tuple(Fd, Sv, Xs) ->
  log(Fd, ?OP_TCONS, ?EMPTY_TAG_ID, [Sv | Xs]).

%% ------------------------------------------------------------------
%% Log binary / bitstring creation.
%% ------------------------------------------------------------------

-spec log_make_bitstring(file:io_device(), cuter_symbolic:symbolic(), any(), integer()) -> ok.
log_make_bitstring(Fd, Sv, V, Sz) ->
  log(Fd, ?OP_MAKE_BITSTR, ?EMPTY_TAG_ID, [Sv, V, Sz]).

%% TODO Use a proper tags.
-spec log_empty_bitstring(file:io_device(), cuter_symbolic:symbolic()) -> ok.
log_empty_bitstring(Fd, Sv) ->
  log(Fd, ?OP_EMPTY_BITSTR, ?EMPTY_TAG_ID, [Sv]).

%% TODO Use a proper tags.
-spec log_nonempty_bitstring(file:io_device(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic()) -> ok.
log_nonempty_bitstring(Fd, H, T, Sv) ->
  log(Fd, ?OP_NONEMPTY_BITSTR, ?EMPTY_TAG_ID, [H, T, Sv]).

%% TODO Use a proper tags.
-spec log_concat_segments(file:io_device(), cuter_symbolic:symbolic(), [any()], any()) -> ok.
log_concat_segments(Fd, Sv1, Bits, Sv) ->
  log(Fd, ?OP_CONCAT_SEGS, ?EMPTY_TAG_ID, [Sv1, Sv | Bits]).

-spec log_bitmatch_const_true(file:io_device(), any(), integer(), any(), cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_const_true(Fd, Cnst, Size, Sv, Sv1, Tag) ->
  log(Fd, ?OP_BITMATCH_CONST_TRUE, cuter_cerl:id_of_tag(Tag), [Sv1, Cnst, Size, Sv]).

-spec log_bitmatch_const_false(file:io_device(), any(), integer(), any(), cuter_cerl:tag()) -> ok.
log_bitmatch_const_false(Fd, Cnst, Size, Sv, Tag) ->
  log(Fd, ?OP_BITMATCH_CONST_FALSE, cuter_cerl:id_of_tag(Tag), [Cnst, Size, Sv]).

-spec log_bitmatch_var_true(file:io_device(), cuter_symbolic:symbolic(), cuter_symbolic:symbolic(), integer(), 
                            cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_var_true(Fd, Sv1, Sv2, Size, Sv, Tag) ->
  log(Fd, ?OP_BITMATCH_VAR_TRUE, cuter_cerl:id_of_tag(Tag), [Sv1, Sv2, Size, Sv]).

-spec log_bitmatch_var_false(file:io_device(), integer(), cuter_symbolic:symbolic(), cuter_cerl:tag()) -> ok.
log_bitmatch_var_false(Fd, Size, Sv, Tag) ->
  log(Fd, ?OP_BITMATCH_VAR_FALSE, cuter_cerl:id_of_tag(Tag), [Size, Sv]).

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
    true  -> log(Fd, ?OP_GUARD_TRUE, cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% False guard
log_guard(Fd, false, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_GUARD_FALSE, cuter_cerl:id_of_tag(Tag), [Sv])
  end.

-spec log_equal(file:io_device(), boolean(), any(), any(), cuter_cerl:tag()) -> ok.
%% Match equal
log_equal(Fd, true, Sv1, Sv2, Tag) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, ?OP_MATCH_EQUAL_TRUE, cuter_cerl:id_of_tag(Tag), [Sv1, Sv2])
  end;
%% Match not equal
log_equal(Fd, false, Sv1, Sv2, Tag) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, ?OP_MATCH_EQUAL_FALSE, cuter_cerl:id_of_tag(Tag), [Sv1, Sv2])
  end.

-spec log_tuple(file:io_device(), (sz | not_sz | not_tpl), any(), integer(), cuter_cerl:tag()) -> ok.
%% Tuple of size N
log_tuple(Fd, sz, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_SZ, cuter_cerl:id_of_tag(Tag), [Sv, N])
  end;
%% Tuple of not size N
log_tuple(Fd, not_sz, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_NOT_SZ, cuter_cerl:id_of_tag(Tag), [Sv, N])
  end;
%% Not a tuple
log_tuple(Fd, not_tpl, Sv, N, Tag) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_NOT_TPL, cuter_cerl:id_of_tag(Tag), [Sv, N])
  end.

-spec log_list(file:io_device(), (nonempty | empty | not_lst), any(), cuter_cerl:tag()) -> ok.
%% Non-empty list
log_list(Fd, nonempty, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_NON_EMPTY, cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% Empty list
log_list(Fd, empty, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_EMPTY, cuter_cerl:id_of_tag(Tag), [Sv])
  end;
%% Not a list
log_list(Fd, not_lst, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_NOT_LST, cuter_cerl:id_of_tag(Tag), [Sv])
  end.

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
      Type = entry_type(OpCode),
      try cuter_json:command_to_json(OpCode, Data) of
        Jdata ->
          write_data(Fd, Type, OpCode, TagID, Jdata)
      catch
        throw:{unsupported_term, _} -> ok
      end
  end.
-else.
log(_, _, _) -> ok.
-endif.

%% Get the JSON Opcode of an MFA.
-spec mfa2op(mfa()) -> integer().
mfa2op(MFA) ->
  dict:fetch(MFA, ?OPCODE_MAPPING).

%% Maps commands to their type
%% (True constraint | False constraint | Everything else)
-spec entry_type(opcode()) -> entry_type().
entry_type(?OP_GUARD_TRUE)  -> ?CONSTRAINT_TRUE;
entry_type(?OP_GUARD_FALSE) -> ?CONSTRAINT_FALSE;
entry_type(?OP_MATCH_EQUAL_TRUE)  -> ?CONSTRAINT_TRUE;
entry_type(?OP_MATCH_EQUAL_FALSE) -> ?CONSTRAINT_FALSE;
entry_type(?OP_TUPLE_SZ)       -> ?CONSTRAINT_TRUE;
entry_type(?OP_TUPLE_NOT_SZ)   -> ?CONSTRAINT_FALSE;
entry_type(?OP_TUPLE_NOT_TPL)  -> ?CONSTRAINT_FALSE;
entry_type(?OP_LIST_NON_EMPTY) -> ?CONSTRAINT_TRUE;
entry_type(?OP_LIST_EMPTY)     -> ?CONSTRAINT_FALSE;
entry_type(?OP_LIST_NOT_LST)   -> ?CONSTRAINT_FALSE;
entry_type(?OP_EMPTY_BITSTR)   -> ?CONSTRAINT_FALSE;
entry_type(?OP_NONEMPTY_BITSTR) -> ?CONSTRAINT_TRUE;
entry_type(?OP_BITMATCH_CONST_TRUE)  -> ?CONSTRAINT_TRUE;
entry_type(?OP_BITMATCH_CONST_FALSE) -> ?CONSTRAINT_FALSE;
entry_type(?OP_BITMATCH_VAR_TRUE)  -> ?CONSTRAINT_TRUE;
entry_type(?OP_BITMATCH_VAR_FALSE) -> ?CONSTRAINT_FALSE;
entry_type(_) -> ?NOT_CONSTRAINT.

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

-spec write_data(file:io_device(), entry_type(), opcode(), cuter_cerl:tagID(), binary()) -> ok.
write_data(Fd, Tp, Op, Tag, Data) when is_integer(Tp), is_integer(Op), is_integer(Tag), is_binary(Data) ->
  Sz = erlang:byte_size(Data),
  ok = file:write(Fd, [Tp, Op, i32_to_list(Tag), i32_to_list(Sz), Data]).

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
  case next_entry(Fd, false) of
    eof -> lists:reverse(Acc);
    {?CONSTRAINT_TRUE, _Op, _TagID}  -> generate_vertex(Fd, [?CONSTRAINT_TRUE_REPR|Acc]);
    {?CONSTRAINT_FALSE, _Op, _TagID} -> generate_vertex(Fd, [?CONSTRAINT_FALSE_REPR|Acc]);
    {?NOT_CONSTRAINT, _Op, _TagID}   -> generate_vertex(Fd, Acc)
  end.

%% Locatse the reversible commands with their tag IDs in a trace file.
-spec locate_reversible(file:name()) -> cuter_analyzer:reversible_with_tags().
locate_reversible(File) ->
  {ok, Fd} = open_file(File, read),
  locate_reversible(Fd, 0, []).

-spec locate_reversible(file:io_device(), non_neg_integer(), cuter_analyzer:reversible_with_tags()) -> cuter_analyzer:reversible_with_tags().
locate_reversible(Fd, N, Acc) ->
  N1 = N + 1,
  case next_entry(Fd, false) of
    eof -> lists:reverse(Acc);
    {?CONSTRAINT_TRUE, _Op, TagID}  -> locate_reversible(Fd, N1, [{N1, TagID}|Acc]);
    {?CONSTRAINT_FALSE, _Op, TagID} -> locate_reversible(Fd, N1, [{N1, TagID}|Acc]);
    {?NOT_CONSTRAINT, Op, TagID} ->
      case is_reversible_operation(Op) of
        false -> locate_reversible(Fd, N, Acc);
        true ->
          case TagID =:= ?EMPTY_TAG_ID of
            true  -> locate_reversible(Fd, N1, Acc);
            false -> locate_reversible(Fd, N1, [{N1, TagID}|Acc])
          end
      end
  end.

%% Returns whether a command can be reversed by the solver or not.
-spec is_reversible_operation(opcode()) -> boolean().
is_reversible_operation(OpCode) ->
  gb_sets:is_member(OpCode, ?REVERSIBLE_OPERATIONS).

-spec next_entry(file:io_device(), true) -> {entry_type(), opcode(), cuter_cerl:tagID(), binary()} | eof
              ; (file:io_device(), false) -> {entry_type(), opcode(), cuter_cerl:tagID()} | eof.
next_entry(Fd, WithData) ->
  case safe_read(Fd, 1, true) of
    eof ->
      close_file(Fd),
      eof;
    <<Tp>> ->
      <<OpCode>> = safe_read(Fd, 1, false),
      TagID = bin_to_i32(safe_read(Fd, 4, false)),
      Sz = bin_to_i32(safe_read(Fd, 4, false)),
      next_entry_data(Fd, WithData, Tp, OpCode, TagID, Sz)
  end.

-spec next_entry_data(file:io_device(), true, entry_type(), opcode(), cuter_cerl:tagID(), integer()) -> {entry_type(), opcode(), cuter_cerl:tagID(), binary()}
                   ; (file:io_device(), false, entry_type(), opcode(), cuter_cerl:tagID(), integer()) -> {entry_type(), opcode(), cuter_cerl:tagID()}.
next_entry_data(Fd, true, Type, OpCode, TagID, Sz) ->
  Data = safe_read(Fd, Sz, false),
  {Type, OpCode, TagID, Data};
next_entry_data(Fd, false, Type, OpCode, TagID, Sz) ->
  {ok, _} = file:position(Fd, {cur, Sz}),
  {Type, OpCode, TagID}.

-spec safe_read(file:io_device(), integer(), boolean()) -> binary() | eof.
safe_read(Fd, Sz, AllowEOF) ->
  case file:read(Fd, Sz) of
    {ok, Bin} -> Bin;
    eof when AllowEOF -> eof;
    eof -> throw(unexpected_eof);
    {error, Reason} -> throw({file_read_failed, Reason})
  end.

