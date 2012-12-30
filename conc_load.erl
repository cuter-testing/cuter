-module(conc_load).

%% External exports
-export([load/2]).

%% Will be using the records representation of
%% the Core Erlang Abstract Syntax Tree
%% as they are defined in core_parse.hrl
-include_lib("compiler/src/core_parse.hrl").

%%====================================================================
%% External exports
%%====================================================================

%load(Mod, Db) ->
%  process_flag(trap_exit, true),
%  Loader = spawn_link(
%    fun() -> 
%      store_module(Mod, Db),
%      exit({ok, Mod})
%    end
%  ),
%  receive
%    {'EXIT', Loader, Msg} ->
%      process_flag(trap_exit, false),
%      Msg
%  end.
  
load(Mod, Db) ->
  store_module(Mod, Db),
  {ok, Mod}.

%%====================================================================
%% Internal functions
%%====================================================================

%% In each ModDb, the following information is saved:
%%
%%       Key                  Value    
%% -----------------    ---------------
%% anno                 ModAnno :: []
%% name                 ModName :: atom()
%% exported             [{Mod :: atom(), Fun :: atom(), Arity :: non_neg_integer()}]  
%% attributes           Attrs :: [{cerl(), cerl()}]
%% {Mod, Fun, Arity}    {Def :: #c_fun{}, Exported :: boolean()}
store_module(Mod, Db) ->

  %% Compile the module to Core Erlang
  Core = compile_core(Mod),
  
  %% Build Core Erlang Abstract Syntax Tree
  {ok, Tokens, _} = scan_file(Core),
  {ok, AST} = core_parse:parse(Tokens),
  
  %% Store Module in the Db
  store_module_info(anno, Mod, AST, Db),
  store_module_info(name, Mod, AST, Db),
  store_module_info(exports, Mod, AST, Db),
  store_module_info(attributes, Mod, AST, Db),
  store_module_funs(Mod, AST, Db),
  
  ok.
  
%% Compile the module source to Core Erlang
compile_core(Mod) ->
  Source = filename:absname(Mod) ++ ".erl",
  compile:file(Source, [to_core]),
  filename:absname(Mod) ++ ".core".
  
%% Core Erlang Scanner
scan_file(File) ->
  {ok, FileContents} = file:read_file(File),
  Data = binary_to_list(FileContents),
  core_scan:string(Data).
  
%% Store Module Information
store_module_info(anno, _Mod, AST, Db) ->
  Anno = AST#c_module.anno,
  ets:insert(Db, {anno, Anno});
%  io:format("[conc_load]: Stored Module Anno: ~p~n", [Anno]);

store_module_info(name, _Mod, AST, Db) ->
  ModName_c = AST#c_module.name,
  ModName = ModName_c#c_literal.val,
  ets:insert(Db, {name, ModName});
%  io:format("[conc_load]: Stored Module Name: ~p~n", [ModName]);
  
store_module_info(exports, Mod, AST, Db) ->
  Exps_c = AST#c_module.exports,
  Fun_info = 
    fun(Elem) ->
      {Fun, Arity} = Elem#c_var.name,
      {Mod, Fun, Arity}
    end,
  Exps = lists:map(Fun_info, Exps_c),
  ets:insert(Db, {exported, Exps});
%  io:format("[conc_load]: Stored Module Exported: ~p~n", [Exps]);
  
store_module_info(attributes, _Mod, AST, Db) ->
  Attrs_c = AST#c_module.attrs,
  ets:insert(Db, {attributes, Attrs_c}).
%  io:format("[conc_load]: Stored Module Attributes: ~p~n", [Attrs_c]).
  
store_module_funs(Mod, AST, Db) ->
  Funs = AST#c_module.defs,
  [{exported, Exps}] = ets:lookup(Db, exported),
  lists:map(fun(X) -> store_fun(Exps, Mod, X, Db) end, Funs).

store_fun(Exps, Mod, {Fun, Def}, Db) ->
  {FunName, Arity} = Fun#c_var.name,
  Exported = lists:member({Mod, FunName, Arity}, Exps),
  ets:insert(Db, {{Mod, FunName, Arity}, {Def, Exported}}).
%  io:format("[conc_load]: Stored Function : ~p:~p/~p (~p) =~n~p~n", [Mod, FunName, Arity, Exported, Def]).
