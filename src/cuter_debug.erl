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
%%   erl -noshell -pa ebin/ -eval "cuter_debug:parse_module(lists)" -s init stop
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
  %io:format("~p~n", [AST]),
  walk(AST, 0).

ident(N) ->
  string:join([ " " || _ <- lists:seq(1, N)], "").

walk(T, N) ->
  Id = ident(N),
  F = fun(X) -> walk(X, N + 2) end,
  case cerl:type(T) of
    module ->
      io:format("~smodule ~p~n", [Id, cerl:concrete(cerl:module_name(T))]),
      F1 = fun(D) -> walk_def(D, N + 2) end,
      lists:foreach(F1, cerl:module_defs(T));
    'fun' ->
      io:format("~sfun: ~n", [Id]),
      io:format("~sVARS~n", [Id]),
      lists:foreach(F, cerl:fun_vars(T)),
      io:format("~sBODY~n", [Id]),
      walk(cerl:fun_body(T), N + 2);
    var ->
      io:format("~svar ~p: ~n", [Id, cerl:var_name(T)]);
    'apply' ->
      io:format("~sapply: ~n", [Id]),
      io:format("~sOP~n", [Id]),
      walk(cerl:apply_op(T), N + 2),
      io:format("~sARGS~n", [Id]),
      lists:foreach(F, cerl:apply_args(T));
    literal ->
      io:format("~sliteral ~p~n", [Id, cerl:concrete(T)]);
    call ->
      io:format("~scall: ~n", [Id]),
      io:format("~sMOD~n", [Id]),
      walk(cerl:call_module(T), N + 2),
      io:format("~sNAME~n", [Id]),
      walk(cerl:call_name(T), N + 2),
      io:format("~sARGS~n", [Id]),
      lists:foreach(F, cerl:call_args(T));
    'case' ->
      io:format("~scase: ~n", [Id]),
      io:format("~sARG~n", [Id]),
      walk(cerl:case_arg(T), N + 2),
      io:format("~sCLAUSES~n", [Id]),
      lists:foreach(F, cerl:case_clauses(T));
    clause ->
      io:format("~sclause: ~n", [Id]),
      io:format("~sPATS~n", [Id]),
      lists:foreach(F, cerl:clause_pats(T)),
      io:format("~sGUARD~n", [Id]),
      walk(cerl:clause_guard(T), N + 2),
      io:format("~sBODY~n", [Id]),
      walk(cerl:clause_body(T), N + 2);
    'let' ->
      io:format("~slet: ~n", [Id]),
      io:format("~sVARS~n", [Id]),
      lists:foreach(F, cerl:let_vars(T)),
      io:format("~sARG~n", [Id]),
      walk(cerl:let_arg(T), N + 2),
      io:format("~sBODY~n", [Id]),
      walk(cerl:let_body(T), N + 2);
    letrec ->
      io:format("~sletrec: ~n", [Id]),
      F2 = fun({X, Y}) -> 
	       io:format("~sDEF~n", [Id]),
	       F(X),
	       io:format("~sARG~n", [Id]),
	       F(Y)
	   end,
      lists:foreach(F2, cerl:letrec_defs(T)),
      io:format("~sBODY~n", [Id]),
      walk(cerl:letrec_body(T), N + 2);
    primop ->
      io:format("~sprimop~n", [Id]);
    Typ ->
      throw({unsupported_cerl, Typ})
  end.

%var_or_lit(T) ->
%  case cerl:type(T) of
%    var -> io_lib:format("~p", [cerl:var_name(T)]);
%    literal -> io_lib:format("~p", [cerl:concrete(T)])
%  end.

walk_def({K, D}, N) ->
  Id = ident(N),
  {F, A} = cerl:var_name(K),
  io:format("~s~p/~p~n", [Id, F, A]),
  walk(D, N + 2).

%% Run as:
%%   erl -noshell -pa ebin/ -eval "cuter_debug:load_annotations(foo, true, \"f.meta\")" -s init stop
-spec load_annotations(module(), boolean(), file:name()) -> cerl:cerl().
load_annotations(M, WithPmatch, F) ->
  Ls = read_lines(F),
  store_lines(Ls),
  {ok, AST} = cuter_cerl:get_core(M, WithPmatch),
  %io:format("~p~n", [AST]),
  walk_with_anno(AST).

-spec walk_with_anno(cerl:cerl()) -> cerl:cerl().
walk_with_anno(T) ->
  F = fun(X) -> walk_with_anno(X) end,
  case cerl:type(T) of
    module ->
      get_line(),
      F1 = fun({A, D}) ->
        get_line(), 
	{A, walk_with_anno(D)}
      end,
      Ds = lists:map(F1, cerl:module_defs(T)),
      cerl:update_c_module(T, cerl:module_name(T), cerl:module_exports(T), cerl:module_attrs(T), Ds);
    'fun' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "fun:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Vars = lists:map(F, cerl:fun_vars(T)),
      get_line(),
      Body = walk_with_anno(cerl:fun_body(T)),
      cerl:update_c_fun(cerl:add_ann([{maybe_error, MaybeError}], T), Vars, Body);
    var ->
      L = get_line(),
      V = string:trim(hd(tl(string:split(L, ":")))),
      MaybeError = not string:is_empty(V),
      cerl:add_ann([{maybe_error, MaybeError}], T);
    'apply' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "apply:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Op = walk_with_anno(cerl:apply_op(T)),
      get_line(),
      Args = lists:map(F, cerl:apply_args(T)),
      cerl:update_c_apply(cerl:add_ann([{maybe_error, MaybeError}], T), Op, Args);
    literal ->
      get_line(),
      cerl:add_ann([{maybe_error, false}], T);
    call ->
      L = get_line(),
      V = string:trim(string:prefix(L, "call:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Mod = walk_with_anno(cerl:call_module(T)),
      get_line(),
      Name = walk_with_anno(cerl:call_name(T)),
      get_line(),
      Args = lists:map(F, cerl:call_args(T)),
      cerl:update_c_call(cerl:add_ann([{maybe_error, MaybeError}], T), Mod, Name, Args);
    'case' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "case:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Arg = walk_with_anno(cerl:case_arg(T)),
      get_line(),
      Clauses = lists:map(F, cerl:case_clauses(T)),
      cerl:update_c_case(cerl:add_ann([{maybe_error, MaybeError}], T), Arg, Clauses);
    clause ->
      L = get_line(),
      V = string:trim(string:prefix(L, "clause:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Pats = lists:map(F, cerl:clause_pats(T)),
      get_line(),
      Guard = walk_with_anno(cerl:clause_guard(T)),
      get_line(),
      Body = walk_with_anno(cerl:clause_body(T)),
      cerl:update_c_clause(cerl:add_ann([{maybe_error, MaybeError}], T), Pats, Guard, Body);
    'let' ->
      L = get_line(),
      V = string:trim(string:prefix(L, "let:")),
      MaybeError = not string:is_empty(V),
      get_line(),
      Vars = lists:map(F, cerl:let_vars(T)),
      get_line(),
      Arg = walk_with_anno(cerl:let_arg(T)),
      get_line(),
      Body = walk_with_anno(cerl:let_body(T)),
      cerl:update_c_let(cerl:add_ann([{maybe_error, MaybeError}], T), Vars, Arg, Body);
    letrec ->
      L = get_line(),
      V = string:trim(string:prefix(L, "letrec:")),
      MaybeError = not string:is_empty(V),
      F2 = fun({X, Y}) -> 
	       get_line(),
	       A = F(X),
	       get_line(),
	       B = F(Y),
	       {A, B}
	   end,
      Defs = lists:map(F2, cerl:letrec_defs(T)),
      get_line(),
      Body = walk_with_anno(cerl:letrec_body(T)),
      cerl:update_c_letrec(cerl:add_ann([{maybe_error, MaybeError}], T), Defs, Body);
    primop ->
      get_line(),
      cerl:add_ann([{maybe_error, true}], T)
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
  _ = file:close(D),
  Lines.

get_all_lines(D, Acc) ->
  case io:get_line(D, "") of
    eof -> lists:reverse(Acc);
    L -> get_all_lines(D, [string:trim(L)|Acc])
  end.

