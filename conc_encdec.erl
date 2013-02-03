-module(conc_encdec).
-compile(export_all).

-define(LOGGING_FLAG, true).  %% Enables logging

i32(B) when is_binary(B) ->
  i32(erlang:binary_to_list(B, 1, 4));
i32([X1, X2, X3, X4]) ->
  (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4;
i32(Int) when is_integer(Int) ->
  [(Int bsr 24) band 255,
   (Int bsr 16) band 255,
   (Int bsr  8) band 255,
    Int band 255].
    
getint32(F) ->
  {ok, B} = file:read(F, 4),
  i32(B).

create_file(Filename) ->
  case file:open(Filename, [write, raw, binary]) of
    {ok, F} ->
      {ok, F};
    {error, Reason} ->
      {error, Reason}
  end.
  
open_file(Filename) ->
  case file:open(Filename, [read, raw, binary]) of
    {ok, F} ->
      {ok, F};
    {error, Reason} ->
      {error, Reason}
  end.
  
close_file(F) ->
  file:close(F).
  
log_term(F, Term) ->
  case ?LOGGING_FLAG of
    true ->
      Bin = erlang:term_to_binary(Term, [{compressed, 1}]),
      Sz = erlang:byte_size(Bin),
      file:write(F, [i32(Sz), Bin]),
      ok;
    false -> ok
  end.

get_term(F) ->
  case file:read(F, 4) of
    {ok, B} ->
      Sz = i32(B),
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


%% ------------------------------------------------------------------------

pprint(F) ->
  case get_term(F) of
    {ok, Term} ->
      pprint_term(Term),
      pprint(F);
    eof -> 
      ok
  end.

pprint_term(T) ->
  case T of
    {pid, Pid} ->
      io:format("$  Pid : ~w~n", [Pid]);
    {'eq', V1, V2} ->
      io:format("$  ~w == ~w~n", [V1, V2]);
    {'neq', V1, V2} ->
      io:format("$  ~w != ~w~n", [V1, V2]);
    {'tuple_elem_eq', V, N} ->
      io:format("$  ~w => tuple and size of ~w~n", [V, N]);
    {'tuple_elem_neq', V, N} ->
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
