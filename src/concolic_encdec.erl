%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_encdec).

%% exports are alphabetically ordered
-export([close_file/1, get_data/1, open_file/2, pprint/1, log_pid/2,
         log/3, log/4, path_vertex/1]).

-define(LOGGING_FLAG, ok).  %% Enables logging

-type mode() :: 'read' | 'write'.

%%====================================================================
%% External exports
%%====================================================================

%% Opens a file for logging or reading terms
-spec open_file(file:name(), mode()) -> {'ok', file:io_device()}.

open_file(F, M) when M =:= 'read'; M =:= 'write' ->
  file:open(F, [M, raw, binary, compressed, {delayed_write, 262144, 2000}]).

%% Wrapper for closing a file
-spec close_file(file:io_device()) -> 'ok'.

close_file(F) ->
  ok = file:close(F).

%% Return the next term stored in a file
-spec get_data(file:io_device()) -> {'ok', {integer(), binary()}} | 'eof'.

get_data(F) ->
  case safe_read(F, 1, true) of
    eof -> eof;
    Id ->
      Sz = bin_to_i32(safe_read(F, 4, false)),
      Bin = safe_read(F, Sz, false),
      {ok, {Id, Bin}}
  end.

%% Store a term in a file
-spec write_data(file:io_device(), term(), binary()) -> 'ok'.

-ifdef(LOGGING_FLAG).
write_data(F, Id, Data) when is_integer(Id), is_binary(Data) ->
  Sz = erlang:byte_size(Data),
  ok = file:write(F, [Id, i32_to_list(Sz), Data]).
-else.
write_data(_F, _Cmd, _Data) ->
  ok.
-endif.

%% Pretty print the terms stored in a file
%% (for debugging purposes only)
-spec pprint(file:io_device()) -> 'ok'.

pprint(F) ->
  case get_data(F) of
    eof -> ok;
    {ok, {Id, Data}} ->
      io:format("~w -> ~p~n", [Id, Data]),
      pprint(F)
  end.

%% ------------------------------------------------------------------
%% Read Data
%% ------------------------------------------------------------------
-spec path_vertex(file:name()) -> concolic_analyzer:path_vertex().

path_vertex(File) ->
  {ok, Fd} = open_file(File, 'read'),
  generate_vertex(Fd, []).

generate_vertex(Fd, Acc) ->
  case safe_read(Fd, 1, true) of
    eof ->
      close_file(Fd),
      lists:reverse(Acc);
    <<N>> ->
      Sz = bin_to_i32(safe_read(Fd, 4, false)),
      {ok, _} = file:position(Fd, {cur, Sz}),
      case N of
        1 -> generate_vertex(Fd, [concolic_analyzer:constraint_true()|Acc]);
        2 -> generate_vertex(Fd, [concolic_analyzer:constraint_false()|Acc]);
        _ -> generate_vertex(Fd, Acc)
      end
  end.

%% ------------------------------------------------------------------
%% Wrappers for write_data/3
%% ------------------------------------------------------------------

-spec log('receive' | 'case', file:io_device(), term(), term()) -> 'ok'.

%% Do not log 'Receive'
log('receive', _Fd, _Type, _Info) ->
  ok;

%% 'Guard True' and 'Guard False' constraints
log('case', Fd, 'guard', {Sv, G}) when is_boolean(G) ->
  case concolic_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log_helper(Fd, {'guard', G}, [Sv])
  end;

%% 'Eq' and 'Neq' constraints
log('case', Fd, M, {V1, V2}) when M =:= 'eq'; M=:= 'neq' ->
  case concolic_symbolic:is_symbolic(V1) orelse concolic_symbolic:is_symbolic(V2) of
    false -> ok;
    true  -> log_helper(Fd, M, [V1, V2])
  end;

%% 'Tuple of Size', 'Tuple of not Size', 'Not Tuple' constraints
log('case', Fd, 'tuple_size', {M, Sv, N}) when (M =:= 'eq' orelse M=:= 'neq') andalso is_integer(N) ->
  case concolic_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log_helper(Fd, {'tuple_size', M}, [Sv, N])
  end;
log('case', Fd, 'not_tuple'=M, {Sv, N}) ->
  case concolic_symbolic:is_symbolic(Sv) of
    false -> ok;
    true  -> log_helper(Fd, M, [Sv, N])
  end;

%% 'Non Empty List', 'Not List' constraints
log('case', Fd, T, V) when T =:= 'non_empty_list'; T =:= 'not_list'; T =:= 'empty_list' ->
  case concolic_symbolic:is_symbolic(V) of
    false -> ok;
    true  -> log_helper(Fd, T, [V])
  end;

%% TODO bitstring matching constraints
log('case', Fd, Bn, Info) when Bn =:= 'match'; Bn =:= 'not_match'; Bn =:= 'not_match_v' ->
  Is = tuple_to_list(Info),
  case lists:any(fun concolic_symbolic:is_symbolic/1, Is) of
    false -> ok;
    true  -> log_helper(Fd, Bn, [Info])
  end.

%% Generic logging function
-spec log(file:io_device(), mfa() | atom(), [term()]) -> 'ok'.

log(Fd, Cmd, Data) when is_list(Data) ->
  case lists:any(fun concolic_symbolic:is_symbolic/1, Data) of
    false -> ok;
    true  -> log_helper(Fd, Cmd, Data)
  end.

log_helper(Fd, Cmd, Data) ->
  Op = json_command_op(Cmd),
  Json_data = concolic_json:command_to_json(Op, Data),
  write_data(Fd, command_type(Cmd), Json_data).

%% Log a pid
-spec log_pid(file:io_device(), pid()) -> 'ok'.

log_pid(Fd, Pid) ->
  write_data(Fd, 2, list_to_binary("PID = " ++ pid_to_list(Pid))).
  
%%====================================================================
%% Internal functions
%%====================================================================

%% Decode a 4-byte binary to the corresponding 32-bit number
-spec bin_to_i32(binary()) -> non_neg_integer().

bin_to_i32(B) when is_binary(B) ->
  [X1, X2, X3, X4] = erlang:binary_to_list(B, 1, 4),
  (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.
  
%% Encode a 32-bit integer to its corresponding sequence of four bytes
-spec i32_to_list(non_neg_integer()) -> [byte(), ...].

i32_to_list(Int) when is_integer(Int) ->
  [(Int bsr 24) band 255,
   (Int bsr 16) band 255,
   (Int bsr  8) band 255,
    Int band 255].

%% Maps commands to their type
%% 1 : True constraint
%% 2 : False constraint
%% 3 : Everything else
command_type({'guard', true}) -> 1;
command_type({'guard', false}) -> 2;
command_type('eq') -> 1;
command_type('neq') -> 2;
command_type({'tuple_size', 'eq'}) -> 1;
command_type({'tuple_size', 'neq'}) -> 2;
command_type('not_tuple') -> 2;
command_type('non_empty_list') -> 1;
command_type('empty_list') -> 2;
command_type('not_list') -> 2;
command_type('match') -> 1;
command_type('not_match') -> 2;
command_type('not_match_v') -> 2;
command_type(_) -> 3.

%% Maps commands to their JSON Opcodes
json_command_op({'guard', true}) -> "T";
json_command_op({'guard', false}) -> "F";
json_command_op('eq') -> "Eq";
json_command_op('neq') -> "Neq";
json_command_op({'tuple_size', 'eq'}) -> "Ts";
json_command_op({'tuple_size', 'neq'}) -> "Nts";
json_command_op('non_empty_list') -> "Nel";
json_command_op('empty_list') -> "El";
json_command_op('not_list') -> "Nl";
json_command_op('not_tuple') -> "Nt";
json_command_op('match') -> "M";
json_command_op('not_match') -> "Nm";
json_command_op('not_match_v') -> "Nmv";
json_command_op('break_list') -> "Bkl";
json_command_op('break_tuple') -> "Bkt";
json_command_op('params') -> "Pms";
json_command_op({erlang, '=:=', 2}) -> "=:=";
json_command_op({erlang, '=/=', 2}) -> "=/=";
json_command_op({erlang, '<', 2}) -> "<";
json_command_op({erlang, '>', 2}) -> ">";
json_command_op({erlang, '>=', 2}) -> ">=";
json_command_op({erlang, '=<', 2}) -> "=<";
json_command_op({erlang, '+', 2}) -> "+";
json_command_op({erlang, '-', 2}) -> "-";
json_command_op({erlang, '*', 2}) -> "*";
json_command_op({erlang, '/', 2}) -> "/";
json_command_op({erlang, 'div', 2}) -> "div";
json_command_op({erlang, 'rem', 2}) -> "rem";
json_command_op({erlang, 'or', 2}) -> "or";
json_command_op({erlang, 'orelse', 2}) -> "ore";
json_command_op({erlang, 'and', 2}) -> "and";
json_command_op({erlang, 'andalso', 2}) -> "anda";
json_command_op({erlang, 'not', 1}) -> "not";
json_command_op({erlang, 'hd', 1}) -> "hd";
json_command_op({erlang, 'tl', 1}) -> "tl";
json_command_op({erlang, 'abs', 1}) -> "abs";
json_command_op({erlang, 'element', 2}) -> "elm";
json_command_op({erlang, 'float', 1}) -> "flt";
json_command_op({erlang, 'is_atom', 1}) -> "isa";
json_command_op({erlang, 'is_boolean', 1}) -> "isb";
json_command_op({erlang, 'is_float', 1}) -> "isf";
json_command_op({erlang, 'is_integer', 1}) -> "isi";
json_command_op({erlang, 'is_list', 1}) -> "isl";
json_command_op({erlang, 'is_number', 1}) -> "isn";
json_command_op({erlang, 'is_tuple', 1}) -> "ist";
json_command_op({erlang, 'round', 1}) -> "rnd";
json_command_op({erlang, 'trunc', 1}) -> "trc".

safe_read(Fd, Sz, AllowEOF) ->
  case file:read(Fd, Sz) of
    {ok, Bin} -> Bin;
    eof when AllowEOF -> eof;
    eof -> exit('unexpected_eof');
    {error, Reason} -> exit({'file_read_failed', Reason})
  end.

