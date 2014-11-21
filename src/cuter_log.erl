%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_log).

-include("cuter_macros.hrl").

-export([
    close_file/1
  , count_reversible/1
  , log_equal/4
  , log_guard/3
  , log_list/3
  , log_mfa/4
  , log_message_consumed/3
  , log_message_received/3
  , log_message_sent/3
  , log_spawn/3
  , log_spawned/3
  , log_symb_params/2
  , log_tuple/4
  , log_unfold_symbolic/4
  , next_entry/2
  , open_file/2
  , path_vertex/1
  , write_data/4
]).

-export_type([opcode/0]).

-type mode()   :: read | write.
-type opcode() :: integer().
-type entry_type() :: ?CONSTRAINT_TRUE | ?CONSTRAINT_FALSE | ?NOT_CONSTRAINT.

%% Opens a file for logging or reading terms
-spec open_file(file:name(), mode()) -> {ok, file:io_device()}.
open_file(F, M) when M =:= read; M =:= write ->
  file:open(F, [M, raw, binary, compressed, {delayed_write, 262144, 2000}]).

%% Closes a file
-spec close_file(file:io_device()) -> ok.
close_file(F) -> ok = file:close(F).

%% ------------------------------------------------------------------
%% Log a symbolic MFA operation
%% ------------------------------------------------------------------

-spec log_mfa(file:io_device(), mfa(), [any()], cuter_symbolic:symbolic()) -> ok.
log_mfa(Fd, MFA, SAs, X) ->
  %% SAs has at least one symbolic argument 
  %% as ensured by cuter_sumbolic:evaluate_mfa/4
  log(Fd, mfa2op(MFA), [X | SAs]).

%% ------------------------------------------------------------------
%% Log Entry Point MFA's parameters & spec
%% ------------------------------------------------------------------

-spec log_symb_params(file:io_device(), [cuter_symbolic:symbolic()]) -> ok.
log_symb_params(_Fd, []) -> ok;
log_symb_params(Fd, Ps)  -> log(Fd, ?OP_PARAMS, Ps).

%% ------------------------------------------------------------------
%% Log process spawns
%% ------------------------------------------------------------------

-spec log_spawn(file:io_device(), pid(), reference()) -> ok.
log_spawn(Fd, Child, Ref) ->
  log(Fd, ?OP_SPAWN, [node(Child), Child, Ref]).

-spec log_spawned(file:io_device(), pid(), reference()) -> ok.
log_spawned(Fd, Parent, Ref) ->
  log(Fd, ?OP_SPAWNED, [node(Parent), Parent, Ref]).

%% ------------------------------------------------------------------
%% Log message passing
%% ------------------------------------------------------------------

-spec log_message_sent(file:io_device(), pid(), reference()) -> ok.
log_message_sent(Fd, Dest, Ref) ->
  log(Fd, ?OP_MSG_SEND, [node(Dest), Dest, Ref]).

-spec log_message_received(file:io_device(), pid(), reference()) -> ok.
log_message_received(Fd, From, Ref) ->
  log(Fd, ?OP_MSG_RECEIVE, [node(From), From, Ref]).

-spec log_message_consumed(file:io_device(), pid(), reference()) -> ok.
log_message_consumed(Fd, From, Ref) ->
  log(Fd, ?OP_MSG_CONSUME, [node(From), From, Ref]).

%% ------------------------------------------------------------------
%% Log the unfolding of a symbolic variable that represents 
%% a list or a tuple of values
%% ------------------------------------------------------------------

-spec log_unfold_symbolic(file:io_device(), (break_tuple | break_list), cuter_symbolic:symbolic(), [cuter_symbolic:symbolic()]) -> ok.
log_unfold_symbolic(Fd, break_tuple, Sv, Vs) ->
  log(Fd, ?OP_UNFOLD_TUPLE, [Sv | Vs]);
log_unfold_symbolic(Fd, break_list, Sv, Vs) ->
  log(Fd, ?OP_UNFOLD_LIST, [Sv | Vs]).

%% ------------------------------------------------------------------
%% Log Constraints
%% ------------------------------------------------------------------

-spec log_guard(file:io_device(), boolean(), any()) -> ok.
%% True guard
log_guard(Fd, true, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_GUARD_TRUE, [Sv])
  end;
%% False guard
log_guard(Fd, false, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_GUARD_FALSE, [Sv])
  end.

-spec log_equal(file:io_device(), boolean(), any(), any()) -> ok.
%% Match equal
log_equal(Fd, true, Sv1, Sv2) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, ?OP_MATCH_EQUAL_TRUE, [Sv1, Sv2])
  end;
%% Match not equal
log_equal(Fd, false, Sv1, Sv2) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, ?OP_MATCH_EQUAL_FALSE, [Sv1, Sv2])
  end.

-spec log_tuple(file:io_device(), (sz | not_sz | not_tpl), any(), integer()) -> ok.
%% Tuple of size N
log_tuple(Fd, sz, Sv, N) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_SZ, [Sv, N])
  end;
%% Tuple of not size N
log_tuple(Fd, not_sz, Sv, N) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_NOT_SZ, [Sv, N])
  end;
%% Not a tuple
log_tuple(Fd, not_tpl, Sv, N) when is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_TUPLE_NOT_TPL, [Sv, N])
  end.

-spec log_list(file:io_device(), (nonempty | empty | not_lst), any()) -> ok.
%% Non-empty list
log_list(Fd, nonempty, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_NON_EMPTY, [Sv])
  end;
%% Empty list
log_list(Fd, empty, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_EMPTY, [Sv])
  end;
%% Not a list
log_list(Fd, not_lst, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, ?OP_LIST_NOT_LST, [Sv])
  end.

%% ------------------------------------------------------------------
%% Logging Function
%% ------------------------------------------------------------------

%% Log data to a file
-spec log(file:io_device(), opcode(), [any()]) -> ok.
-ifdef(LOGGING_FLAG).
log(Fd, OpCode, Data) ->
  case get(?DEPTH_PREFIX) of
    undefined -> throw(depth_undefined_in_pdict);
    0 -> ok;
    N when is_integer(N), N > 0 ->
      Type = entry_type(OpCode),
      try cuter_json:command_to_json(OpCode, Data) of
        Jdata ->
          write_data(Fd, Type, OpCode, Jdata),
          update_constraint_counter(Type, N)
      catch
        throw:{unsupported_term, _} -> ok
      end
  end.
-else.
log(_, _, _) -> ok.
-endif.

%% Maps MFAs to their JSON Opcodes
mfa2op({erlang,       hd,         1}) -> ?OP_HD;
mfa2op({erlang,       tl,         1}) -> ?OP_TL;
mfa2op({erlang,       is_integer, 1}) -> ?OP_IS_INTEGER;
mfa2op({erlang,       is_atom,    1}) -> ?OP_IS_ATOM;
mfa2op({erlang,       is_float,   1}) -> ?OP_IS_FLOAT;
mfa2op({erlang,       is_list,    1}) -> ?OP_IS_LIST;
mfa2op({erlang,       is_tuple,   1}) -> ?OP_IS_TUPLE;
mfa2op({erlang,       is_boolean, 1}) -> ?OP_IS_BOOLEAN;
mfa2op({erlang,       is_number,  1}) -> ?OP_IS_NUMBER;
mfa2op({erlang,       '+',        2}) -> ?OP_PLUS;
mfa2op({erlang,       '-',        2}) -> ?OP_MINUS;
mfa2op({erlang,       '*',        2}) -> ?OP_TIMES;
mfa2op({erlang,       '/',        2}) -> ?OP_RDIV;
mfa2op({cuter_erlang, pos_div,    2}) -> ?OP_IDIV_NAT;
mfa2op({cuter_erlang, pos_rem,    2}) -> ?OP_REM_NAT;
mfa2op({erlang,       '-',        1}) -> ?OP_UNARY;
mfa2op({erlang,       '=:=',      2}) -> ?OP_EQUAL;
mfa2op({erlang,       float,      1}) -> ?OP_FLOAT.

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
entry_type(_) -> ?NOT_CONSTRAINT.

%% Reduce the contraint counter by one every time
%% a constraint is logged.
-spec update_constraint_counter(entry_type(), integer()) -> ok.
update_constraint_counter(Type, N) when Type =:= ?CONSTRAINT_TRUE; Type =:= ?CONSTRAINT_FALSE ->
  _ = put(?DEPTH_PREFIX, N-1), ok;
update_constraint_counter(_Type, _ok) -> ok.

%% ------------------------------------------------------------------
%% Read / Write Data
%% ------------------------------------------------------------------

-spec write_data(file:io_device(), entry_type(), opcode(), binary()) -> ok.
write_data(Fd, Tp, Op, Data) when is_integer(Tp), is_integer(Op), is_binary(Data) ->
  Sz = erlang:byte_size(Data),
  ok = file:write(Fd, [Tp, Op, i32_to_list(Sz), Data]).

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
    {?CONSTRAINT_TRUE, _Tp}  -> generate_vertex(Fd, [?CONSTRAINT_TRUE_REPR|Acc]);
    {?CONSTRAINT_FALSE, _Tp} -> generate_vertex(Fd, [?CONSTRAINT_FALSE_REPR|Acc]);
    {?NOT_CONSTRAINT, _Tp}   -> generate_vertex(Fd, Acc)
  end.

%% Count the reversible commands in a trace file
-spec count_reversible(file:name()) -> integer().
count_reversible(File) ->
  {ok, Fd} = open_file(File, read),
  count_reversible(Fd, 0).

-spec count_reversible(file:io_device(), integer()) -> integer().
count_reversible(Fd, Acc) ->
  case next_entry(Fd, false) of
    eof -> Acc;
    {?CONSTRAINT_TRUE, _Tp}  -> count_reversible(Fd, Acc + 1);
    {?CONSTRAINT_FALSE, _Tp} -> count_reversible(Fd, Acc + 1);
    {?NOT_CONSTRAINT, Op} ->
      case is_reversible_operation(Op) of
        true  -> count_reversible(Fd, Acc + 1);
        false -> count_reversible(Fd, Acc)
      end
  end.

-spec is_reversible_operation(opcode()) -> boolean().
is_reversible_operation(?OP_HD)       -> true;
is_reversible_operation(?OP_TL)       -> true;
is_reversible_operation(?OP_PLUS)     -> true;
is_reversible_operation(?OP_MINUS)    -> true;
is_reversible_operation(?OP_TIMES)    -> true;
is_reversible_operation(?OP_RDIV)     -> true;
is_reversible_operation(?OP_IDIV_NAT) -> true;
is_reversible_operation(?OP_REM_NAT)  -> true;
is_reversible_operation(?OP_FLOAT)    -> true;
is_reversible_operation(_) -> false.

-spec next_entry(file:io_device(), true) -> {entry_type(), opcode(), binary()} | eof
              ; (file:io_device(), false) -> {entry_type(), opcode()} | eof.
next_entry(Fd, WithData) ->
  case safe_read(Fd, 1, true) of
    eof ->
      close_file(Fd),
      eof;
    <<N>> ->
      <<Tp>> = safe_read(Fd, 1, false),
      Sz = bin_to_i32(safe_read(Fd, 4, false)),
      next_entry_data(Fd, WithData, N, Tp, Sz)
  end.

-spec next_entry_data(file:io_device(), true, entry_type(), opcode(), integer()) -> {entry_type(), opcode(), binary()}
                   ; (file:io_device(), false, entry_type(), opcode(), integer()) -> {entry_type(), opcode()}.
next_entry_data(Fd, true, Type, OpCode, Sz) ->
  Data = safe_read(Fd, Sz, false),
  {Type, OpCode, Data};
next_entry_data(Fd, false, Type, OpCode, Sz) ->
  {ok, _} = file:position(Fd, {cur, Sz}),
  {Type, OpCode}.

-spec safe_read(file:io_device(), integer(), boolean()) -> binary() | eof.
safe_read(Fd, Sz, AllowEOF) ->
  case file:read(Fd, Sz) of
    {ok, Bin} -> Bin;
    eof when AllowEOF -> eof;
    eof -> throw(unexpected_eof);
    {error, Reason} -> throw({file_read_failed, Reason})
  end.

