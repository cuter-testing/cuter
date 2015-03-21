%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl).

%% External exports
-export([load/4, retrieve_spec/2, get_tag/1, get_next_tag/1, id_of_tag/1, tag_from_id/1, empty_tag/0]).

%% Will be using the records representation of the Core Erlang Abstract Syntax Tree
%% as they are defined in core_parse.hrl
-include_lib("compiler/src/core_parse.hrl").
-include("include/cuter_macros.hrl").

-export_type([compile_error/0, cerl_spec/0, cerl_func/0, cerl_type/0, cerl_bounded_func/0,
              tagID/0, tag/0, tag_generator/0]).

-type info()          :: anno | attributes | exports | name.
-type compile_error() :: {error, {loaded_ret_atoms(), module()}} 
                       | {runtime_error, {compile, {atom(), term()}}}.

-type tagID() :: integer().
-type tag() :: {?BRANCH_TAG_PREFIX, tagID()}.
-type tag_generator() :: fun(() -> tag()).

-type lineno() :: integer().
-type cerl_attr() :: {cerl:c_literal(), cerl:c_literal()}.
-type cerl_func() :: {type, lineno(), 'fun', [cerl_product() | cerl_type()]}.
-type cerl_bounded_func() :: {type, lineno(), bounded_fun, [cerl_func() | cerl_constraint()]}.
-type cerl_constraint() :: {type, lineno(), constraint, [{atom, lineno(), is_subtype} | [atom() | cerl_type()]]}.
-type cerl_spec() :: [cerl_func() | cerl_bounded_func(), ...].

-type cerl_product() :: {type, lineno(), product, [cerl_type()]}.
-type cerl_base_type() :: nil | any | term | boolean | integer | float.
-type cerl_type() :: {atom, lineno(), atom()}
                   | {integer, lineno(), integer()}
                   | {type, lineno(), cerl_base_type(), []}
                   | {type, lineno(), list, [cerl_type()]}
                   | {type, lineno(), tuple, any | [cerl_type()]}
                   | {type, lineno(), union, [cerl_type()]}.

%%====================================================================
%% External exports
%%====================================================================
-spec load(module(), ets:tid(), nonempty_string(), tag_generator()) -> {ok, module()} | compile_error().
load(Mod, Db, Dir, TagGen) ->
  try store_module(Mod, Db, Dir, TagGen) of
    ok -> {ok, Mod}
  catch
    throw:non_existing      -> {error, {non_existing, Mod}};
    throw:preloaded         -> {error, {preloaded, Mod}};
    throw:cover_compiled    -> {error, {cover_compiled, Mod}};
    throw:{compile, Errors} -> {runtime_error, {compile, {Mod, Errors}}}
  end.

%% Retrieves the spec of a function from a stored module's info.
-spec retrieve_spec(ets:tid(), {atom(), integer()}) -> cerl_spec() | not_found.
retrieve_spec(Db, FA) ->
  [{attributes, Attrs}] = ets:lookup(Db, attributes),
  locate_spec(Attrs, FA).

%% Locates the spec of a function from the list of the module's attributes.
-spec locate_spec([cerl_attr()], {atom(), integer()}) -> cerl_spec() | not_found.
locate_spec([], _FA) ->
  not_found;
locate_spec([{#c_literal{val = spec}, #c_literal{val = [{FA, Spec}]}}|_Attrs], FA) ->
  Spec;
locate_spec([_|Attrs], FA) ->
  locate_spec(Attrs, FA).

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
-spec store_module(module(), ets:tid(), nonempty_string(), tag_generator()) -> ok.
store_module(M, Db, Dir, TagGen) ->
  Core = compile_core(M, Dir),          %% Compile the module to Core Erlang
  {ok, Tokens, _} = scan_file(Core),    %% Build Core Erlang Abstract Syntax Tree
  {ok, AST} = core_parse:parse(Tokens), %% Store Module in the Db
  store_module_info(anno, M, AST, Db),
  store_module_info(name, M, AST, Db),
  store_module_info(exports, M, AST, Db),
  store_module_info(attributes, M, AST, Db),
  store_module_funs(M, AST, Db, TagGen),
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
  FileName = filename:absname(atom_to_list(M) ++ ".core", Dir),
  ok = filelib:ensure_dir(FileName),
  {ok, BeamPath} = ensure_mod_loaded(M),
  {ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(BeamPath, [compile_info]),
  Source = proplists:get_value(source, Info),
  Includes = proplists:lookup_all(i, proplists:get_value(options, Info)),
  Macros = proplists:lookup_all(d, proplists:get_value(options, Info)),
  CompInfo = [to_core, return_errors, {outdir, Dir}] ++ Includes ++ Macros,
  CompRet = compile:file(Source, CompInfo),
  case CompRet of
    {ok, M} -> FileName;
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
-spec store_module_funs(module(), cerl:cerl(), ets:tab(), tag_generator()) -> ok.
store_module_funs(M, AST, Db, TagGen) ->
  Funs = AST#c_module.defs,
  [{exported, Exps}] = ets:lookup(Db, exported),
  lists:foreach(fun(X) -> store_fun(Exps, M, X, Db, TagGen) end, Funs).

%% Store the AST of a Function
-spec store_fun([atom()], module(), {cerl:c_var(), cerl:c_fun()}, ets:tab(), tag_generator()) -> ok.
store_fun(Exps, M, {Fun, Def}, Db, TagGen) ->
  {FunName, Arity} = Fun#c_var.name,
  MFA = {M, FunName, Arity},
  Exported = lists:member(MFA, Exps),
  AnnDef = annotate(Def, TagGen),
  true = ets:insert(Db, {MFA, {AnnDef, Exported}}),
  ok.

%% Annotate the clauses with tags.
-spec annotate(cerl:cerl(), tag_generator()) -> cerl:cerl().
annotate({c_alias, Anno, Var, Pat}, TagGen) ->
  {c_alias, Anno, annotate(Var, TagGen), annotate(Pat, TagGen)};
annotate({c_apply, Anno, Op, Args}, TagGen) ->
  {c_apply, Anno, annotate(Op, TagGen), [annotate(A, TagGen) || A <- Args]};
annotate({c_binary, Anno, Segs}, TagGen) ->
  {c_binary, Anno, [annotate(S, TagGen) || S <- Segs]};
annotate({c_bitstr, Anno, Val, Sz, Unit, Type, Flags}, TagGen) ->
  {c_bitstr, Anno, annotate(Val, TagGen), annotate(Sz, TagGen),
    annotate(Unit, TagGen), annotate(Type, TagGen), annotate(Flags, TagGen)};
annotate({c_call, Anno, Mod, Name, Args}, TagGen) ->
  {c_call, Anno, annotate(Mod, TagGen), annotate(Name, TagGen), [annotate(A, TagGen) || A <- Args]};
annotate({c_case, Anno, Arg, Clauses}, TagGen) ->
  Cs0 = [annotate(Cl, TagGen) || Cl <- Clauses],
  Cs1 = annotate_next_clause(Cs0),
  {c_case, Anno, annotate(Arg, TagGen), Cs1};
annotate({c_catch, Anno, Body}, TagGen) ->
  {c_catch, Anno, annotate(Body, TagGen)};
annotate({c_clause, Anno, Pats, Guard, Body}, TagGen) ->
  {c_clause, [TagGen()|Anno], [annotate(P, TagGen) || P <- Pats], annotate(Guard, TagGen), annotate(Body, TagGen)};
annotate({c_cons, Anno, Hd, Tl}, TagGen) ->
  {c_cons, Anno, annotate(Hd, TagGen), annotate(Tl, TagGen)};
annotate({c_fun, Anno, Vars, Body}, TagGen) ->
  {c_fun, Anno, [annotate(V, TagGen) || V <- Vars], annotate(Body, TagGen)};
annotate({c_let, Anno, Vars, Arg, Body}, TagGen) ->
  {c_let, Anno, [annotate(V, TagGen) || V <- Vars], annotate(Arg, TagGen), annotate(Body, TagGen)};
annotate({c_letrec, Anno, Defs, Body}, TagGen) ->
  {c_letrec, Anno, [{annotate(X, TagGen), annotate(Y, TagGen)} || {X, Y} <- Defs], annotate(Body, TagGen)};
annotate({c_literal, Anno, Val}, _TagGen) ->
  {c_literal, Anno, Val};
annotate({c_primop, Anno, Name, Args}, TagGen) ->
  {c_primop, Anno, annotate(Name, TagGen), [annotate(A, TagGen) || A <- Args]};
annotate({c_receive, Anno, Clauses, Timeout, Action}, TagGen) ->
  Cs0 = [annotate(Cl, TagGen) || Cl <- Clauses],
  Cs1 = annotate_next_clause(Cs0),
  {c_receive, Anno, Cs1, annotate(Timeout, TagGen), annotate(Action, TagGen)};
annotate({c_seq, Anno, Arg, Body}, TagGen) ->
  {c_seq, Anno, annotate(Arg, TagGen), annotate(Body, TagGen)};
annotate({c_try, Anno, Arg, Vars, Body, Evars, Handler}, TagGen) ->
  {c_try, Anno, annotate(Arg, TagGen), [annotate(V, TagGen) || V <- Vars],
    annotate(Body, TagGen), [annotate(EV, TagGen) || EV <- Evars], annotate(Handler, TagGen)};
annotate({c_tuple, Anno, Es}, TagGen) ->
  {c_tuple, Anno, [annotate(E, TagGen) || E <- Es]};
annotate({c_values, Anno, Es}, TagGen) ->
  {c_values, Anno, [annotate(E, TagGen) || E <- Es]};
annotate({c_var, Anno, Name}, _TagGen) ->
  {c_var, Anno, Name}.

%% Annotates clauses with next tags.
annotate_next_clause(Clauses) ->
  annotate_next_clause(Clauses, []).

annotate_next_clause([Clause], Acc) ->
  lists:reverse([Clause | Acc]);
annotate_next_clause([C1, C2 | Cs], Acc) ->
  Tag2 = get_tag(C2),
  NC1 = add_next_tag(C1, Tag2),
  annotate_next_clause([C2 | Cs], [NC1 | Acc]).

%% Gets the tag of a clause.
-spec get_tag(cerl:c_clause()) -> tag() | none.
get_tag(#c_clause{anno=Anno}) ->
  get_tag_from_anno(Anno).

-spec get_tag_from_anno(list()) -> tag() | none.
get_tag_from_anno([]) ->
  none;
get_tag_from_anno([{?BRANCH_TAG_PREFIX, _N}=Tag | _Annos]) ->
  Tag;
get_tag_from_anno([_|Annos]) ->
  get_tag_from_anno(Annos).

%% Adds a next tag to a clause.
-spec add_next_tag(cerl:c_clause(), tag()) -> cerl:c_clause().
add_next_tag(#c_clause{anno=Anno}=C, Tag) ->
  C#c_clause{anno=[{next_tag, Tag}|Anno]}.

%% Gets the next tag of clause.
-spec get_next_tag(cerl:c_clause()) -> tag() | none.
get_next_tag(#c_clause{anno=Anno}) ->
  get_next_tag_from_anno(Anno).

-spec get_next_tag_from_anno(list()) -> tag() | none.
get_next_tag_from_anno([]) ->
  none;
get_next_tag_from_anno([{next_tag, Tag={?BRANCH_TAG_PREFIX, _N}} | _Annos]) ->
  Tag;
get_next_tag_from_anno([_|Annos]) ->
  get_next_tag_from_anno(Annos).

%% Create a tag from tag info.
-spec tag_from_id(tagID()) -> tag().
tag_from_id(N) ->
  {?BRANCH_TAG_PREFIX, N}.

%% Get the tag info of a tag.
-spec id_of_tag(tag()) -> tagID().
id_of_tag({?BRANCH_TAG_PREFIX, N}) -> N.

%% Creates an empty tag.
-spec empty_tag() -> tag().
empty_tag() -> tag_from_id(?EMPTY_TAG_ID).
