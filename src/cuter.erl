%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/4]).

-include("cuter_macros.hrl").

-spec run(module(), atom(), [any()], pos_integer()) -> ok.
run(M, F, As, Depth) ->
  error_logger:tty(false),  %% Disable error_logger
  io:format("Testing ~p:~p/~p ...~n", [M, F, length(As)]),
  ok.
