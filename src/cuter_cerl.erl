%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl).

%% External exports
-export([load/3]).

%% Will be using the records representation of the Core Erlang Abstract Syntax Tree
%% as they are defined in core_parse.hrl
-include_lib("compiler/src/core_parse.hrl").
-include("cuter_macros.hrl").

-export_type([compile_error/0]).

-type info()          :: anno | attributes | exports | name.
-type compile_error() :: {error, {loaded_ret_atoms(), module()}} 
                       | {runtime_error, {compile, {atom(), term()}}}.

%%====================================================================
%% External exports
%%====================================================================
-spec load(module(), ets:tid(), nonempty_string()) -> {ok, module()} | compile_error().
load(Mod, Db, Dir) ->
  try store_module(Mod, Db, Dir) of
    ok -> {ok, Mod}
  catch
    throw:non_existing      -> {error, {non_existing, Mod}};
    throw:preloaded         -> {error, {preloaded, Mod}};
    throw:cover_compiled    -> {error, {cover_compiled, Mod}};
    throw:{compile, Errors} -> {runtime_error, {compile, {Mod, Errors}}}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%% In each ModDb, the following information is saved:
%% 
%%       Key                  Value    
%% -----------------    ---------------
%% anno                 Anno :: []
%% name                 Name :: module()
%% exported             [{M :: module(), Fun :: atom(), Arity :: non_neg_integer()}]  
%% attributes           Attrs :: [{cerl(), cerl()}]
%% {M, Fun, Arity}      {Def :: #c_fun{}, Exported :: boolean()}
-spec store_module(module(), ets:tid(), nonempty_string()) -> ok.
store_module(M, Db, Dir) ->
  Core = compile_core(M, Dir),          %% Compile the module to Core Erlang
  {ok, Tokens, _} = scan_file(Core),    %% Build Core Erlang Abstract Syntax Tree
  {ok, AST} = core_parse:parse(Tokens), %% Store Module in the Db
  store_module_info(anno, M, AST, Db),
  store_module_info(name, M, AST, Db),
  store_module_info(exports, M, AST, Db),
  store_module_info(attributes, M, AST, Db),
  store_module_funs(M, AST, Db),
  ok.

%% Core Erlang Scanner
-spec scan_file(file:filename()) -> term().
scan_file(File) ->
  {ok, FileContents} = file:read_file(File),
  Data = binary_to_list(FileContents),
  core_scan:string(Data).

%% Compile the module source to Core Erlang
-spec compile_core(module(), nonempty_string()) -> file:filename().
compile_core(M, Dir) ->
  ok = filelib:ensure_dir(Dir ++ "/"),
  {ok, BeamPath} = ensure_mod_loaded(M),
  {ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(BeamPath, [compile_info]),
  Source = proplists:get_value(source, Info),
  Includes = proplists:lookup_all(i, proplists:get_value(options, Info)),
  Macros = proplists:lookup_all(d, proplists:get_value(options, Info)),
  CompInfo = [to_core, return_errors, {outdir, Dir}] ++ Includes ++ Macros,
  CompRet = compile:file(Source, CompInfo),
  case CompRet of
    {ok, M} -> Dir ++ "/" ++ atom_to_list(M) ++ ".core";
    Errors  -> erlang:throw({compile, Errors})
  end.
  
%% Ensure the module beam code is loaded
%% and return the path it is located
-spec ensure_mod_loaded(module()) -> {ok, file:filename()} | no_return().
ensure_mod_loaded(M) ->
  case code:which(M) of
    non_existing   -> erlang:throw(non_existing);
    preloaded      -> erlang:throw(preloaded);
    cover_compiled -> erlang:throw(cover_compiled);
    Path -> {ok, Path}
  end.
  
%% Store Module Information
-spec store_module_info(info(), module(), cerl:cerl(), ets:tab()) -> ok.
store_module_info(anno, _M, AST, Db) ->
  Anno = AST#c_module.anno,
  true = ets:insert(Db, {anno, Anno}),
  ok;
store_module_info(attributes, _M, AST, Db) ->
  Attrs_c = AST#c_module.attrs,
  true = ets:insert(Db, {attributes, Attrs_c}),
  ok;
store_module_info(exports, M, AST, Db) ->
  Exps_c = AST#c_module.exports,
  Fun_info = 
    fun(Elem) ->
      {Fun, Arity} = Elem#c_var.name,
      {M, Fun, Arity}
    end,
  Exps = [Fun_info(E) || E <- Exps_c],
  true = ets:insert(Db, {exported, Exps}),
  ok;
store_module_info(name, _M, AST, Db) ->
  ModName_c = AST#c_module.name,
  ModName = ModName_c#c_literal.val,
  true = ets:insert(Db, {name, ModName}),
  ok.

%% Store Module exported functions
-spec store_module_funs(module(), cerl:cerl(), ets:tab()) -> ok.
store_module_funs(M, AST, Db) ->
  Funs = AST#c_module.defs,
  [{exported, Exps}] = ets:lookup(Db, exported),
  lists:foreach(fun(X) -> store_fun(Exps, M, X, Db) end, Funs).

%% Store the AST of a Function
-spec store_fun([atom()], module(), {cerl:c_var(), cerl:c_fun()}, ets:tab()) -> ok.
store_fun(Exps, M, {Fun, Def}, Db) ->
  {FunName, Arity} = Fun#c_var.name,
  MFA = {M, FunName, Arity},
  Exported = lists:member(MFA, Exps),
  true = ets:insert(Db, {MFA, {Def, Exported}}),
  ok.
