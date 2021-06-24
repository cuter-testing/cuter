%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_debug).
-export([parse_module_specs/2, parse_module/2, minimal_ast/2, load_annotations/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parses the types & specs of a module.
%% Run as:
%%   erl -noshell -eval "cuter_debug:parse_module_specs(crypto, true)" -s init stop

-spec parse_module_specs(module(), boolean()) -> ok.
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

%% Run as:
%%   erl -noshell -pa ebin/ -eval "cuter_debug:minimal_ast(foo, true)" -s init stop
-spec minimal_ast(module(), boolean()) -> ok.
minimal_ast(M, WithPmatch) ->
  {ok, AST} = cuter_cerl:get_core(M, WithPmatch),
  walk(AST, 0).

ident(N) ->
  string:join([ " " || _ <- lists:seq(1, N)], "").

walk(T, N) ->
  Id = ident(N),
  case cerl:type(T) of
    module ->
      io:format("~smodule ~p~n", [Id, cerl:concrete(cerl:module_name(T))]),
      F = fun(D) -> walk_def(D, N + 2) end,
      lists:foreach(F, cerl:module_defs(T));
    'fun' ->
      io:format("~sfun: ~n", [Id]),
      io:format("~sVARS~n", [Id]),
      F = fun(X) -> walk(X, N + 2) end,
      lists:foreach(F, cerl:fun_vars(T)),
      io:format("~sBODY~n", [Id]),
      walk(cerl:fun_body(T), N + 2);
    var ->
      io:format("~svar ~p~n", [Id, cerl:var_name(T)]);
    'apply' ->
      io:format("~sapply: ~n", [Id]),
      io:format("~sOP~n", [Id]),
      walk(cerl:apply_op(T), N + 2),
      io:format("~sARGS~n", [Id]),
      F = fun(X) -> walk(X, N + 2) end,
      lists:foreach(F, cerl:apply_args(T));
    literal ->
      io:format("~sliteral ~p~n", [Id, cerl:concrete(T)]);
    call ->
      io:format("~scall: ~n", [Id]),
      io:format("~sMFA ~s:~s/~p~n", [Id, var_or_lit(cerl:call_module(T)),
		          	     var_or_lit(cerl:call_name(T)),
				     length(cerl:call_args(T))]),
      io:format("~sARGS~n", [Id]),
      F = fun(X) -> walk(X, N + 2) end,
      lists:foreach(F, cerl:call_args(T))
  end.

var_or_lit(T) ->
  case cerl:type(T) of
    var -> io_lib:format("~p", [cerl:var_name(T)]);
    literal -> io_lib:format("~p", [cerl:concrete(T)])
  end.

walk_def({K, D}, N) ->
  Id = ident(N),
  {F, A} = cerl:var_name(K),
  io:format("~s~p/~p~n", [Id, F, A]),
  walk(D, N + 2).

%% Run as:
%%   erl -noshell -pa ebin/ -eval "cuter_debug:load_annotations(foo, true, \"f.meta\")" -s init stop
-spec load_annotations(module(), boolean(), file:name()) -> ok.
load_annotations(M, WithPmatch, F) ->
  Ls = read_lines(F),
  store_lines(Ls),
  {ok, AST} = cuter_cerl:get_core(M, WithPmatch),
  walk_with_anno(AST),
  eof = get_line().

walk_with_anno(T) ->
  case cerl:type(T) of
    module ->
      get_line(),
      F = fun({_, D}) ->
        get_line(), 
	walk_with_anno(D)
      end,
      lists:foreach(F, cerl:module_defs(T));
    'fun' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "fun:")),
      case string:is_empty(V) of
        true -> ok;
        false -> io:format("FUN ANNOTATION: ~p~n", [V])
      end,
      get_line(),
      F = fun(X) -> walk_with_anno(X) end,
      lists:foreach(F, cerl:fun_vars(T)),
      get_line(),
      walk_with_anno(cerl:fun_body(T));
    var ->
      get_line();
    'apply' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "apply:")),
      case string:is_empty(V) of
        true -> ok;
        false -> io:format("APPLY ANNOTATION: ~p~n", [V])
      end,
      get_line(),
      walk_with_anno(cerl:apply_op(T)),
      get_line(),
      F = fun(X) -> walk_with_anno(X) end,
      lists:foreach(F, cerl:apply_args(T));
    literal ->
      get_line();
    call ->
      L = get_line(),
      V = string:trim(string:prefix(L, "call:")),
      case string:is_empty(V) of
        true -> ok;
        false -> io:format("CALL ANNOTATION: ~p~n", [V])
      end,
      get_line(),
      get_line(),
      F = fun(X) -> walk_with_anno(X) end,
      lists:foreach(F, cerl:call_args(T))
  end.

store_lines(Ls) -> put('__ann_lines', Ls).

get_line() ->
  case get('__ann_lines') of
    [] ->
      eof;
    [H|T] ->
      store_lines(T),
      H
  end.


read_lines(Fname) ->
  {ok, D} = file:open(Fname, [read]),
  Lines = get_all_lines(D, []),
  file:close(D),
  Lines.

get_all_lines(D, Acc) ->
  case io:get_line(D, "") of
    eof -> lists:reverse(Acc);
    L -> get_all_lines(D, [string:trim(L)|Acc])
  end.
