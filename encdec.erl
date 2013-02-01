-module(encdec).
-compile(export_all).

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
  Bin = erlang:term_to_binary(Term),
  Sz = erlang:byte_size(Bin),
  case file:write(F, [i32(Sz), Bin]) of
    ok ->
      ok;
    {error, Reason} ->
      exit({file_write_failed, Reason})
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


run() ->
  File = "trace",
  {ok, F} = create_file(File),
  lists:foldl(
    fun(T, Fd) -> log_term(Fd, T), io:format("x"), Fd end,
    F, generator()
  ),
  close_file(F),
  io:format("File done!~n"),
  {ok, RF} = open_file(File),
  print(RF),
  close_file(RF).
  
print(F) ->
  case get_term(F) of
    {ok, Term} ->
      io:format("~w~n", [Term]),
      print(F);
    eof -> 
      ok
  end.
  
generator() ->
  [
    {test, test, test},
    lists:seq(1,10),
    {<<45,6>>, ok},
    foo
  ].
