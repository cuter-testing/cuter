%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_debug).
-export([parse_module_specs/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parses the types & specs of a module.
%% Run as:
%%   erl -noshell -eval "cuter_debug:parse_module_specs(crypto, true)" -s init stop

-spec parse_module_specs(atom(), boolean()) -> ok.
parse_module_specs(Module, WithPmatch) ->
  Attrs = get_module_attrs(Module, WithPmatch),
  {TypeAttrs, SpecAttrs} = cuter_cerl:classify_attributes(Attrs),
  io:format("[**] Classified Attributes~n"),
  _Types = cuter_types:retrieve_types(TypeAttrs),
  io:format("[**] Retrieved Types~n"),
  _Specs = cuter_types:retrieve_specs(SpecAttrs),
  io:format("[**] Retrieved Specs~n"),
  ok.

get_module_attrs(Module, WithPmatch) ->
  Beam = code:which(Module),
  {ok, {Module, [{abstract_code, {_, AbstractCode}}]}} = beam_lib:chunks(Beam, [abstract_code]),
  {ok, Module, AST} = compile:forms(AbstractCode, compile_options(WithPmatch)),
  cerl:module_attrs(AST).

compile_options(true) -> [to_core, {core_transform, cerl_pmatch}];
compile_options(false) -> [to_core].


