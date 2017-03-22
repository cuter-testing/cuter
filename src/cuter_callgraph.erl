%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_callgraph).

-export([get_callgraph/2, foldModule/3, foreachModule/2, get_visited_mfas/1]).

-export_type([callgraph/0]).

-type fn_cache() :: dict:dict({atom(), arity()}, cerl:cerl()).
-type module_cache_entry() :: fn_cache() | preloaded.
-type module_cache() :: dict:dict(atom(), module_cache_entry()).
-type visited_mfas() :: gb_sets:set(mfa()).
-type visited_modules() :: sets:set(cuter:mod()).

-record(callgraph, {
  visited_modules :: visited_modules(),
  visited_mfas    :: visited_mfas()
}).
-opaque callgraph() :: #callgraph{}.

-record(conf, {whitelist :: cuter_mock:whitelist()}).
%-type configuration() :: #conf{}.

%% ----------------------------------------------------------------------------
%% Callgraph type API.
%% ----------------------------------------------------------------------------

-spec foreachModule(fun((cuter:mod()) -> any()), callgraph()) -> ok.
foreachModule(Fn, Callgraph) ->
  FoldFn = fun(M, _) -> Fn(M), ok end,
  foldModule(FoldFn, ok, Callgraph).

-spec foldModule(fun((cuter:mod(), any()) -> any()), any(), callgraph()) -> ok.
foldModule(Fn, Acc0, #callgraph{visited_modules = VisitedModules}) ->
  sets:fold(Fn, Acc0, VisitedModules).

-spec get_visited_mfas(callgraph()) -> visited_mfas().
get_visited_mfas(#callgraph{visited_mfas = VisitedMfas}) -> VisitedMfas.

%% ----------------------------------------------------------------------------
%% Configuration API.
%% ----------------------------------------------------------------------------

mk_conf(Whitelist) ->
  #conf{whitelist = Whitelist}.

%% ----------------------------------------------------------------------------
%% Callgraph calculation.
%% ----------------------------------------------------------------------------

-spec get_callgraph([mfa()], cuter_mock:whitelist()) -> {ok, callgraph()} | {error, string()}.
get_callgraph(Mfas, Whitelist) ->
  try
    cuter_pp:callgraph_calculation_started(Mfas),
    Conf = mk_conf(Whitelist),
    {VisitedMfas, _} = get_callgraph_all(Mfas, gb_sets:empty(), dict:new(), Conf),
    VisitedMods = visited_modules(VisitedMfas),
    cuter_pp:callgraph_calculation_succeeded(),
    {ok, #callgraph{visited_modules = VisitedMods, visited_mfas = VisitedMfas}}
  catch
    throw:Reason ->
      cuter_pp:callgraph_calculation_failed(Reason),
      {error, Reason}
  end.

-spec visited_modules(visited_mfas()) -> visited_modules().
visited_modules(Visited) ->
  Fn = fun({M,_,_}, Acc) -> sets:add_element(M, Acc) end,
  gb_sets:fold(Fn, sets:new(), Visited).

get_callgraph_all([], Visited, Modules, _Conf) ->
  {Visited, Modules};
get_callgraph_all([Mfa|Rest], Visited, Modules, Conf) ->
  {Visited1, Modules1} = get_callgraph_one(Mfa, Visited, Modules, Conf),
  get_callgraph_all(Rest, Visited1, Modules1, Conf).

get_callgraph_one({M,F,A}=OrigMfa, Visited, ModuleCache, Conf) ->
  case cuter_mock:simulate_behaviour(M, F, A) of
    bif ->
      UpdatedVisited = gb_sets:add_element(OrigMfa, Visited),
      {UpdatedVisited, ModuleCache};
    {ok, {SM,_,_}=Mfa} ->
      UpdatedVisited = gb_sets:add_element(Mfa, Visited),
      case cuter_mock:is_whitelisted(Mfa, Conf#conf.whitelist) of
        %% Do not expand whitelisted mfas.
        true ->
          {UpdatedVisited, ModuleCache};
        false ->
          case get_definition(Mfa, Visited, ModuleCache) of
            {nodef, UpdatedModuleCache} ->
              {UpdatedVisited, UpdatedModuleCache};
            {def, Def, UpdatedModuleCache} ->
              expand(SM, Def, UpdatedVisited, UpdatedModuleCache, ordsets:new(), Conf)
          end
      end
  end.

expand(M, Tree, Visited, ModuleCache, LetrecFuns, Conf) ->
  case cerl:type(Tree) of
    %% c_fun
    'fun' ->
      expand(M, cerl:fun_body(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_apply
    'apply' ->
      Op = cerl:apply_op(Tree),
      Args = cerl:apply_args(Tree),
      case is_fname(Op) of
        true ->
          {F,A} = cerl:var_name(Op),
          case ordsets:is_element({F,A}, LetrecFuns) of
            true -> expand_all(M, Args, Visited, ModuleCache, LetrecFuns, Conf);
            false ->
              {NewV, NewM} = expand_all(M, Args, Visited, ModuleCache, LetrecFuns, Conf),
              Mfa = {M, F, A},
              get_callgraph_one(Mfa, NewV, NewM, Conf)
          end;
        false ->
          Trees = [Op | Args],
          expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf)
      end;
    %% c_let
    'let' ->
      Trees = [cerl:let_arg(Tree), cerl:let_body(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_letrec
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      NewLetrecFuns = ordsets:union([ordsets:from_list([cerl:var_name(Fn) || {Fn, _} <- Defs]), LetrecFuns]),
      Trees = [Def || {_, Def} <- Defs] ++ [cerl:letrec_body(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, NewLetrecFuns, Conf);
    %% c_call
    call ->
      Mod = cerl:call_module(Tree),
      F = cerl:call_name(Tree),
      A = cerl:call_arity(Tree),
      Args = cerl:call_args(Tree),
      case is_mfname(Mod, F) of
        true ->
          Mfa = {cerl:concrete(Mod), cerl:concrete(F), A},
          {NewV, NewM} = expand_all(M, Args, Visited, ModuleCache, LetrecFuns, Conf),
          get_callgraph_one(Mfa, NewV, NewM, Conf);
        false ->
          Trees = [Mod, F] ++ Args,
          expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf)
      end;
    %% c_try
    'try' ->
      Trees = [cerl:try_arg(Tree), cerl:try_body(Tree), cerl:try_handler(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_receive
    'receive' ->
      Trees = cerl:receive_clauses(Tree) ++ [cerl:receive_timeout(Tree), cerl:receive_action(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_case
    'case' ->
      Trees = [cerl:case_arg(Tree) | cerl:case_clauses(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_clause
    clause ->
      Trees = [cerl:clause_guard(Tree), cerl:clause_body(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_cons
    cons ->
      Trees = [cerl:cons_hd(Tree), cerl:cons_tl(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_tuple
    tuple ->
      expand_all(M, cerl:tuple_es(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_catch
    'catch' ->
      expand(M, cerl:catch_body(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_binary
    binary ->
      expand_all(M, cerl:binary_segments(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_bitstr
    bitstr ->
      Trees = [cerl:bitstr_val(Tree), cerl:bitstr_size(Tree), cerl:bitstr_unit(Tree),
        cerl:bitstr_type(Tree), cerl:bitstr_flags(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_map
    map ->
      Trees = [cerl:map_arg(Tree) | cerl:map_es(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_map_pair
    map_pair ->
      Trees = [cerl:map_pair_key(Tree), cerl:map_pair_val(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_seq
    seq ->
      Trees = [cerl:seq_arg(Tree), cerl:seq_body(Tree)],
      expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf);
    %% c_primop
    primop ->
      expand_all(M, cerl:primop_args(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_values
    values ->
      expand_all(M, cerl:values_es(Tree), Visited, ModuleCache, LetrecFuns, Conf);
    %% c_literal
    literal ->
      {Visited, ModuleCache};
    %% c_var
    var ->
      case cerl:var_name(Tree) of
        {F,A} when is_atom(F), is_integer(A) ->
          get_callgraph_one({M,F,A}, Visited, ModuleCache, Conf);
        _ -> {Visited, ModuleCache}
      end;
    %% c_alias
    alias ->
      {Visited, ModuleCache}
  end.

expand_all(M, Trees, Visited, ModuleCache, LetrecFuns, Conf) ->
  Fn = fun(Tree, {Vs, Cache}) -> expand(M, Tree, Vs, Cache, LetrecFuns, Conf) end,
  lists:foldl(Fn, {Visited, ModuleCache}, Trees).

is_mfname(M, F) ->
  cerl:is_literal(M) andalso cerl:is_literal(F) andalso
    cerl:is_literal_term(M) andalso cerl:is_literal_term(F) andalso
    is_atom(cerl:concrete(M)) andalso is_atom(cerl:concrete(F)).

is_fname(Tree) ->
  case cerl:is_c_var(Tree) of
    false -> false;
    true ->
      case cerl:var_name(Tree) of
        {F,A} when is_atom(F) andalso is_integer(A) -> true;
        _ -> false
      end
  end.

%% ----------------------------------------------------------------------------
%% Get the definition of MFAs.
%% ----------------------------------------------------------------------------

-spec get_definition(mfa(), visited_mfas(), module_cache()) -> {nodef, module_cache()} | {def, cerl:cerl(), module_cache()}.
get_definition({M,F,A}=Mfa, Visited, ModuleCache) ->
  case gb_sets:is_member(Mfa, Visited) of
    true -> {nodef, ModuleCache};  %% Have seen this MFA before.
    false ->
      case erlang:is_builtin(M, F, A) or lists:member(M, cuter_mock:overriding_modules()) of
        true -> {nodef, ModuleCache};  %% It's a BIF.
        false ->
          case lookup_definition(Mfa, ModuleCache) of
            {preloaded, UpdatedModuleCache} -> {nodef, UpdatedModuleCache};
            {Def, UpdatedModuleCache} -> {def, Def, UpdatedModuleCache}
          end
      end
  end.

-spec lookup_definition(mfa(), module_cache()) -> {cerl:cerl() | preloaded, module_cache()}.
lookup_definition({M,F,A}=Mfa, ModuleCache) ->
  case fetch_safe(M, ModuleCache) of
    preloaded -> {preloaded, ModuleCache};
    ennoent ->
      UpdatedModuleCache = cache_module(M, ModuleCache),
      %% This won't loop forever as if the module doesn't
      %% exist, an exception will be thrown.
      lookup_definition(Mfa, UpdatedModuleCache);
    FnCache ->
      case fetch_safe({F,A}, FnCache) of
        ennoent -> throw(cuter_pp:non_existing_mfa(Mfa));
        Def -> {Def, ModuleCache}
      end
  end.

-spec cache_module(atom(), module_cache()) -> module_cache().
cache_module(M, ModuleCache) ->
  case get_core(M) of
    preloaded -> dict:store(M, preloaded, ModuleCache);
    Ast ->
      Defs = cerl:module_defs(Ast),
      FnCache = lists:foldl(fun({Fn, Def}, Acc) -> dict:store(cerl:var_name(Fn), Def, Acc) end,
        dict:new(), Defs),
      dict:store(M, FnCache, ModuleCache)
  end.

-spec get_core(cuter:mod()) -> cerl:cerl() | preloaded.
get_core(M) ->
  case cuter_cerl:get_core(M, false) of
    {error, preloaded} -> preloaded;
    {error, Error} -> throw(Error);
    {ok, Ast} -> Ast
  end.

fetch_safe(Key, Dict) ->
  try dict:fetch(Key, Dict)
  catch error:badarg -> ennoent
  end.
