-module(conc_load).

%% External exports
-export([load/3]).

%% Will be using the records representation of
%% the Core Erlang Abstract Syntax Tree
%% as they are defined in core_parse.hrl
-include_lib("compiler/src/core_parse.hrl").

%%====================================================================
%% External exports
%%====================================================================
  
load(Mod, Db, Dir) ->
  try store_module(Mod, Db, Dir) of
    ok -> {ok, Mod}
  catch
    throw:non_existing ->
      {error, {non_existing, Mod}};
    throw:preloaded ->
      {error, {preloaded, Mod}};
    throw:cover_compiled ->
      {error, {cover_compiled, Mod}};
    throw:{compile, Errors} ->
      {error, {compile, {Mod, Errors}}}
  end.

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
store_module(Mod, Db, Dir) ->

  %% Compile the module to Core Erlang
  Core = compile_core(Mod, Dir),
  
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
  
%% Core Erlang Scanner
scan_file(File) ->
  {ok, FileContents} = file:read_file(File),
  Data = binary_to_list(FileContents),
  core_scan:string(Data).
  
%% Compile the module source to Core Erlang
compile_core(Mod, Dir) ->
  ok = filelib:ensure_dir(Dir ++ "/"),
  {ok, BeamPath} = ensure_mod_loaded(Mod),
  {ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(BeamPath, [compile_info]),
  Source = proplists:get_value(source, Info),
  Includes = proplists:lookup_all(i, proplists:get_value(options, Info)),
  Macros = proplists:lookup_all(d, proplists:get_value(options, Info)),
  CompInfo = [to_core, return_errors, {outdir, Dir}] ++ Includes ++ Macros,
  CompRet = compile:file(Source, CompInfo),
  case CompRet of
    {ok, Mod} ->
      Dir ++ "/" ++ atom_to_list(Mod) ++ ".core";
    Errors ->
      erlang:throw({compile, Errors})
  end.
  
ensure_mod_loaded(Mod) ->
  case code:which(Mod) of
    non_existing ->
      erlang:throw(non_existing);
    preloaded ->
      erlang:throw(preloaded);
    cover_compiled ->
      erlang:throw(cover_compiled);
    Path ->
      {ok, Path}
  end.
  
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
