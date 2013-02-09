%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_encdec).

%% exports are alphabetically ordered
-export([close_file/1, get_term/1, open_file/2, pprint/1, log_pid/2,
         log_guard/3, log_eq/4, log_tuple_size/4, log_type/3]).

-define(LOGGING_FLAG, ok).  %% Enables logging

-type mode() :: 'read' | 'write'.

%%====================================================================
%% External exports
%%====================================================================

%% Opens a file for logging or reading terms
-spec open_file(file:name(), mode()) -> {'ok', file:io_device()}.

open_file(F, M) when M =:= read; M =:= write ->
  file:open(F, [M, raw, binary, {delayed_write, 262144, 2000}]).

%% Wrapper for closing a file
-spec close_file(file:io_device()) -> 'ok'.

close_file(F) ->
  ok = file:close(F).

%% Return the next term stored in the file
-spec get_term(file:io_device()) -> {'ok', term()} | 'eof'.

 get_term(F) ->
   case file:read(F, 4) of
     {ok, B} ->
       Sz = bin_to_i32(B),
       case file:read(F, Sz) of
         {ok, Bin} ->
           Term = erlang:binary_to_term(Bin),
           {ok, Term};
         eof ->
           exit(unexpected_eof);
         {error, Reason} ->
           exit({file_read_failed, Reason})
       end;
     eof ->
       eof;
     {error, Reason} ->
       exit({file_read_failed, Reason})
   end.

%% Store a term in the file
-spec log_term(file:io_device(), term()) -> 'ok'.

-ifdef(LOGGING_FLAG).
log_term(F, Term) ->
%  erlang:display(Term),
  Bin = erlang:term_to_binary(Term, [{compressed, 1}]),
  Sz = erlang:byte_size(Bin),
  ok = file:write(F, [i32_to_list(Sz), Bin]).
-else.
log_term(_F, _Term) ->
  ok.
-endif.

%% Pretty print the terms stored in a file
%% (for debugging purposes only)
-spec pprint(file:io_device()) -> 'ok'.

pprint(F) ->
 case get_term(F) of
   {ok, Term} ->
     pprint_term(Term),
     pprint(F);
   eof -> 
     ok
 end.
 
%% ------------------------------------------------------------------
%% Wrappers for log_term/2
%% ------------------------------------------------------------------

-spec log_pid(file:io_device(), pid()) -> 'ok'.

log_pid(Fd, Pid) ->
  log_term(Fd, {'pid', Pid}).

-spec log_eq(file:io_device(), 'eq' | 'neq', term(), term()) -> 'ok'.

log_eq(Fd, M, V1, V2) when M =:= 'eq'; M =:= 'neq' ->
  case concolic_symbolic:is_symbolic(V1) orelse concolic_symbolic:is_symbolic(V2) of
    true  -> log_term(Fd, {M, V1, V2});
    false -> 'ok'
  end.


-spec log_guard(file:io_device(), term(), boolean()) -> 'ok'.

log_guard(Fd, V, Gv) when is_boolean(Gv) ->
  case {V =:= Gv, Gv} of
    {true, _} -> 'ok';
    {false, true}  -> log_term(Fd, {'guard_true', V});
    {false, false} -> log_term(Fd, {'guard_false', V}) 
  end.
  
-spec log_tuple_size(file:io_device(), 'eq' | 'neq', term(), integer()) -> 'ok'.
log_tuple_size(Fd, M, Tup, Sz) when M =:= 'eq'; M =:= 'neq' ->
  case {concolic_symbolic:is_symbolic(Tup), M} of
    {true, 'eq'}  -> log_term(Fd, {'tuple_size_eq', Tup, Sz});
    {true, 'neq'} -> log_term(Fd, {'tuple_size_neq', Tup, Sz});
    {false, _} -> 'ok'
  end.
  
-spec log_type(file:io_device(), 'non_empty_list' | 'not_list' | 'not_tuple', term()) -> 'ok'.
  
log_type(Fd, T, V) when T =:= 'non_empty_list'; T =:= 'not_list'; T =:= 'not_tuple' ->
  case concolic_symbolic:is_symbolic(V) of
    true  -> log_term(Fd, {T, V});
    false -> 'ok'
  end.
  
%%====================================================================
%% Internal functions
%%====================================================================

%% Decode a 4-byte binary ro the corresponding 32-bit number
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

%% Pretty print a logging term
%% (used in pprint/1 - for debugging purposes only)
-spec pprint_term(term()) -> 'ok'.
pprint_term(T) ->
 case T of
   {pid, Pid} ->
     io:format("$  Pid : ~w~n", [Pid]);
   {'eq', V1, V2} ->
     io:format("$  ~w == ~w~n", [V1, V2]);
   {'neq', V1, V2} ->
     io:format("$  ~w != ~w~n", [V1, V2]);
   {'tuple_size_eq', V, N} ->
     io:format("$  ~w => tuple and size of ~w~n", [V, N]);
   {'tuple_size_neq', V, N} ->
     io:format("$  ~w => tuple but size not ~w~n", [V, N]);
   {'non_empty_list', V} ->
     io:format("$  ~w => non empty list~n", [V]);
   {'not_tuple', V} ->
     io:format("$  ~w => not tuple~n", [V]);
   {'not_list', V} ->
     io:format("$  ~w => not list~n", [V]);
   {'guard_true', V} ->
     io:format("$  ~w => true guard~n", [V]);
   {'guard_false', V} ->
     io:format("$  ~w => false guard~n", [V]);
   {msg, Msg} ->
     io:format("$  ~p => msg~n", [Msg])
 end.
