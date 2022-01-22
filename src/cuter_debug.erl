%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_debug).
-export([parse_module_specs/2, parse_module/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parses the types & specs of a module.
%% Run as:
%%   erl -noshell -eval "cuter_debug:parse_module_specs(crypto, true)" -s init stop

-spec parse_module_specs(module(), boolean()) -> ok.
parse_module_specs(_Module, _WithPmatch) ->
  ok.

%% Prints the AST of a module.
%% Run as:
%%   erl -noshell -pa ebin/ -eval "cuter_debug:parse_module(lists)" -s init stop
-spec parse_module(module(), boolean()) -> ok.
parse_module(M, WithPmatch) ->
	case cuter_cerl:get_core(M, WithPmatch) of
    {error, E} ->
      io:format("ERROR: ~p~n", [E]);
    {ok, AST} ->
      io:format("~p~n", [AST])
  end.
