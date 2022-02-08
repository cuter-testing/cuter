%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_debug).
-export([parse_module/2]).

%% Prints the AST of a module.
%% Run as:
%%   erl -noshell -pa ebin/ -eval "cuter_debug:parse_module(lists, true)" -s init stop
-spec parse_module(module(), boolean()) -> ok.
parse_module(M, WithPmatch) ->
	case cuter_cerl:get_core(M, WithPmatch) of
    {error, E} ->
      io:format("ERROR: ~p~n", [E]);
    {ok, AST} ->
      io:format("~p~n", [AST])
  end.
