%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_pp).

-export([mfa/3, input/1, exec_status/1, exec_info/1]).

-spec mfa(atom(), atom(), integer()) -> ok.
mfa(M, F, Ar) -> io:format("Testing ~p:~p/~p ...~n", [M, F, Ar]).

-spec input([any()]) -> ok.
input(As) when is_list(As) ->
  lists:foreach(fun(_) -> io:format("=") end, lists:seq(1,50)),
  io:format("~nINPUT~n"),
  input_parameters(As).

input_parameters([]) -> io:format("N/A~n");
input_parameters([H|T]) ->
  io:format("  ~p", [H]),
  lists:foreach(fun(X) -> io:format(", ~p", [X]) end, T),
  io:format("~n").

-spec exec_status(cuter_iserver:execution_status()) -> ok.
exec_status({success, {Cv, Sv}}) ->
  io:format("OK~n"),
  io:format("  CONCRETE: ~p~n", [Cv]),
  io:format("  SYMBOLIC: ~p~n", [Sv]);
exec_status({runtime_error, Node, Pid, {Cv, Sv}}) ->
  io:format("RUNTIME ERROR~n"),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  PID: ~p~n", [Pid]),
  io:format("  CONCRETE: ~p~n", [Cv]),
  io:format("  SYMBOLIC: ~p~n", [Sv]);
exec_status({internal_error, Type, Node, Why}) ->
  io:format("INTERNAL ERROR~n"),
  io:format("  TYPE: ~p~n", [Type]),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  REASON: ~p~n", [Why]).

-spec exec_info(orddict:orddict()) -> ok.
exec_info(Info) ->
  io:format("EXECUTION INFO~n"),
  F = fun({Node, Data}) ->
    io:format("  ~p~n", [Node]),
    lists:foreach(fun exec_info_data/1, Data)
  end,
  lists:foreach(F, Info).

exec_info_data({mapping, Ms}) ->
  io:format("    MAPPING~n"),
  lists:foreach(fun({Sv, Cv}) -> io:format("      ~p <=> ~p~n", [Sv, Cv]) end, Ms);
exec_info_data({monitor_logs, Logs}) ->
  io:format("    MONITOR LOGS~n"),
  io:format("      ~p~n", [Logs]);
exec_info_data({code_logs, Logs}) ->
  io:format("    CODE LOGS~n"),
  io:format("      ~p~n", [Logs]);
exec_info_data(_) -> ok.

