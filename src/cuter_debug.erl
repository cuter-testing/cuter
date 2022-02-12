%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_debug).

-export([parse_module/2, to_erl_types_specs/1]).

%% This modules contains convenience MFAs for debugging purposes during the
%% development of the tool.

%% Prints the AST of a module.
-spec parse_module(module(), boolean()) -> ok.
parse_module(M, WithPmatch) ->
	case cuter_cerl:get_core(M, WithPmatch) of
    {error, E} ->
      io:format("ERROR: ~p~n", [E]);
    {ok, AST} ->
      io:format("~p~n", [AST])
  end.

%% Returns the specs of a list of modules as erl_types representation.
-spec to_erl_types_specs([module()]) -> ok.
to_erl_types_specs(Modules) ->
  Fn = fun(M) ->
    {ok, AST} = cuter_cerl:get_core(M, false),
    AST
    end,
  Xs = [{M, Fn(M)} || M <- Modules],
  TagGen = fun() -> ok end,
  Kmodules = [cuter_cerl:kmodule(M, AST, TagGen) || {M, AST} <- Xs],
  Specs = cuter_types:convert_specs(Kmodules),
  lists:foreach(fun print_mfa_and_spec/1, dict:to_list(Specs)).

print_mfa_and_spec({MFA, Spec}) ->
  io:format("~p~n  ~p~n", [MFA, Spec]).
