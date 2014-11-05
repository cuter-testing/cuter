%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_log).

-include("cuter_macros.hrl").

-export([open_file/2, close_file/1, log_spawn/3, log_spawned/3, log_message_sent/3,
         log_message_received/3, log_symb_params/2, log_guard/3, log_equal/4,
         log_tuple/4, log_list/3, log_unfold_symbolic/4, log_mfa/4,
         path_vertex/1, next_entry/2]).

-type mode() :: read | write.

%% Opens a file for logging or reading terms
-spec open_file(file:name(), mode()) -> {ok, file:io_device()}.
open_file(F, M) when M =:= read; M =:= write ->
  file:open(F, [M, raw, binary, compressed, {delayed_write, 262144, 2000}]).

%% Wrapper for closing a file
-spec close_file(file:io_device()) -> ok.
close_file(F) -> ok = file:close(F).

%% ------------------------------------------------------------------
%% Log a symbolic MFA operation
%% ------------------------------------------------------------------

-spec log_mfa(file:io_device(), mfa(), [any()], cuter_symbolic:symbolic()) -> ok.
log_mfa(Fd, MFA, SAs, X) ->
  %% SAs has at least one symbolic argument 
  %% as ensured by cuter_sumbolic:evaluate_mfa/4
  log(Fd, MFA, [X | SAs]).

%% ------------------------------------------------------------------
%% Log Entry Point MFA's parameters & spec
%% ------------------------------------------------------------------

-spec log_symb_params(file:io_device(), [cuter_symbolic:symbolic()]) -> ok.
log_symb_params(_Fd, []) -> ok;
log_symb_params(Fd, Ps)  -> log(Fd, params, Ps).

%% ------------------------------------------------------------------
%% Log process spawns
%% ------------------------------------------------------------------

-spec log_spawn(file:io_device(), pid(), reference()) -> ok.
log_spawn(Fd, Child, Ref) ->
  log(Fd, spawn, [node(Child), Child, Ref]).

-spec log_spawned(file:io_device(), pid(), reference()) -> ok.
log_spawned(Fd, Parent, Ref) ->
  log(Fd, spawned, [node(Parent), Parent, Ref]).

%% ------------------------------------------------------------------
%% Log message passing
%% ------------------------------------------------------------------

-spec log_message_sent(file:io_device(), pid(), reference()) -> ok.
log_message_sent(Fd, Dest, Ref) ->
  log(Fd, send_msg, [node(Dest), Dest, Ref]).

-spec log_message_received(file:io_device(), pid(), reference()) -> ok.
log_message_received(Fd, From, Ref) ->
  log(Fd, receive_msg, [node(From), From, Ref]).

%% ------------------------------------------------------------------
%% Log the unfolding of a symbolic variable that represents 
%% a list or a tuple of values
%% ------------------------------------------------------------------

-spec log_unfold_symbolic(file:io_device(), (break_tuple | break_list), cuter_symbolic:symbolic(), [cuter_symbolic:symbolic()]) -> ok.
log_unfold_symbolic(Fd, break_tuple, Sv, Vs) ->
  log(Fd, {unfold, tuple}, [Sv | Vs]);
log_unfold_symbolic(Fd, break_list, Sv, Vs) ->
  log(Fd, {unfold, list}, [Sv | Vs]).

%% ------------------------------------------------------------------
%% Log Constraints
%% ------------------------------------------------------------------

-spec log_guard(file:io_device(), boolean(), any()) -> ok.
log_guard(Fd, Tp, Sv) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, {guard, Tp}, [Sv])
  end.

-spec log_equal(file:io_device(), boolean(), any(), any()) -> ok.
log_equal(Fd, Tp, Sv1, Sv2) ->
  case cuter_symbolic:is_symbolic(Sv1) orelse cuter_symbolic:is_symbolic(Sv2) of
    false -> ok;
    true  -> log(Fd, {equal, Tp}, [Sv1, Sv2])
  end.

-spec log_tuple(file:io_device(), (sz | not_sz | not_tpl), any(), integer()) -> ok.
log_tuple(Fd, Tp, Sv, N) when (Tp =:= sz orelse Tp =:= not_sz orelse Tp =:= not_tpl) andalso is_integer(N) ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, {tuple, Tp}, [Sv, N])
  end.

-spec log_list(file:io_device(), (nonempty | empty | not_lst), any()) -> ok.
log_list(Fd, Tp, Sv) when Tp =:= nonempty orelse Tp =:= empty orelse Tp =:= not_lst ->
  case cuter_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log(Fd, {list, Tp}, [Sv])
  end.

%% ------------------------------------------------------------------
%% Logging Function
%% ------------------------------------------------------------------

-ifdef(LOGGING_FLAG).
log(Fd, Cmd, Data) ->
  case get(?DEPTH_PREFIX) of
    undefined -> throw(depth_undefined_in_pdict);
    0 -> ok;
    N when is_integer(N), N > 0 ->
      Op = cmd2op(Cmd),
      CmdType = cmd_type(Cmd),
      try cuter_json:command_to_json(Op, Data) of
        Json_data ->
          write_data(Fd, CmdType, Op, Json_data),
          update_constraint_counter(CmdType, N)
      catch
        throw:{unsupported_term, _} -> ok
      end
  end.
-else.
log(_, _, _) -> ok.
-endif.

%% Maps commands to their JSON Opcodes
cmd2op(params) -> ?OP_PARAMS;
cmd2op(spec)   -> ?OP_SPEC;
cmd2op({guard, true})  -> ?OP_GUARD_TRUE;
cmd2op({guard, false}) -> ?OP_GUARD_FALSE;
cmd2op({equal, true})  -> ?OP_MATCH_EQUAL_TRUE;
cmd2op({equal, false}) -> ?OP_MATCH_EQUAL_FALSE;
cmd2op({tuple, sz})    -> ?OP_TUPLE_SZ;
cmd2op({tuple, not_sz})  -> ?OP_TUPLE_NOT_SZ;
cmd2op({tuple, not_tpl}) -> ?OP_TUPLE_NOT_TPL;
cmd2op({list, nonempty}) -> ?OP_LIST_NON_EMPTY;
cmd2op({list, empty})    -> ?OP_LIST_EMPTY;
cmd2op({list, not_lst})  -> ?OP_LIST_NOT_LST;
cmd2op(spawn)       -> ?OP_SPAWN;
cmd2op(spawned)     -> ?OP_SPAWNED;
cmd2op(send_msg)    -> ?OP_MSG_SEND;
cmd2op(receive_msg) -> ?OP_MSG_RECEIVE;
cmd2op({unfold, tuple}) -> ?OP_UNFOLD_TUPLE;
cmd2op({unfold, list})  -> ?OP_UNFOLD_LIST;
cmd2op({erlang, hd, 1}) -> ?OP_ERLANG_HD_1;
cmd2op({erlang, tl, 1}) -> ?OP_ERLANG_TL_1.

%% Maps commands to their type
%% (True constraint | False constraint | Everything else)
cmd_type({guard, true})  -> ?CONSTRAINT_TRUE;
cmd_type({guard, false}) -> ?CONSTRAINT_FALSE;
cmd_type({equal, true})  -> ?CONSTRAINT_TRUE;
cmd_type({equal, false}) -> ?CONSTRAINT_FALSE;
cmd_type({tuple, sz})    -> ?CONSTRAINT_TRUE;
cmd_type({tuple, not_sz})  -> ?CONSTRAINT_FALSE;
cmd_type({tuple, not_tpl}) -> ?CONSTRAINT_FALSE;
cmd_type({list, nonempty}) -> ?CONSTRAINT_TRUE;
cmd_type({list, empty})    -> ?CONSTRAINT_FALSE;
cmd_type({list, not_lst})  -> ?CONSTRAINT_FALSE;
cmd_type(_) -> ?NOT_CONSTRAINT.

update_constraint_counter(Cmd, N) when Cmd =:= ?CONSTRAINT_TRUE; Cmd =:= ?CONSTRAINT_FALSE ->
  _ = put(?DEPTH_PREFIX, N-1), ok;
update_constraint_counter(_Cmd, _ok) -> ok.

%% ------------------------------------------------------------------
%% Read / Write Data
%% ------------------------------------------------------------------

write_data(Fd, Id, Op, Data) when is_integer(Id), is_integer(Op), is_binary(Data) ->
  Sz = erlang:byte_size(Data),
  ok = file:write(Fd, [Id, Op, i32_to_list(Sz), Data]).

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

-spec next_entry(file:io_device(), true) -> {integer(), integer(), binary()} | eof
              ; (file:io_device(), false) -> {integer(), integer()} | eof.
next_entry(Fd, WithData) ->
  case safe_read(Fd, 1, true) of
    eof ->
      close_file(Fd),
      eof;
    <<N>> ->
      Tp = safe_read(Fd, 1, false),
      Sz = bin_to_i32(safe_read(Fd, 4, false)),
      next_entry_data(Fd, WithData, N, Tp, Sz)
  end.

-spec next_entry_data(file:io_device(), true, integer(), integer(), integer()) -> {integer(), integer(), binary()}
                   ; (file:io_device(), false, integer(), integer(), integer()) -> {integer(), integer()}.
next_entry_data(Fd, true, N, Tp, Sz) ->
  Data = safe_read(Fd, Sz, false),
  {N, Tp, Data};
next_entry_data(Fd, false, N, Tp, Sz) ->
  {ok, _} = file:position(Fd, {cur, Sz}),
  {N, Tp}.

-spec safe_read(file:io_device(), integer(), boolean()) -> binary().
safe_read(Fd, Sz, AllowEOF) ->
  case file:read(Fd, Sz) of
    {ok, Bin} -> Bin;
    eof when AllowEOF -> eof;
    eof -> throw(unexpected_eof);
    {error, Reason} -> throw({file_read_failed, Reason})
  end.

