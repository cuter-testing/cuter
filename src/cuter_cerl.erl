%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl).

%% External exports
-export([load/3, retrieve_spec/2, get_tags/1, id_of_tag/1, tag_from_id/1, empty_tag/0]).

%% We are using the records representation of the Core Erlang Abstract
%% Syntax Tree as they are defined in core_parse.hrl
-include_lib("compiler/src/core_parse.hrl").

-include("include/cuter_macros.hrl").

-export_type([compile_error/0, cerl_spec/0, cerl_func/0, cerl_type/0,
              cerl_bounded_func/0, cerl_constraint/0, tagID/0, tag/0, tag_generator/0,
              cerl_attr_type/0, cerl_recdef/0, cerl_typedef/0, cerl_record_field/0, cerl_type_record_field/0]).

-type info()          :: anno | attributes | exports | name.
-type code_error()    :: {error, {loaded_ret_atoms(), cuter:mod()}}.
-type abs_code_error():: {error, {missing_abstract_code, cuter:mod()}}.
-type compile_error() :: {error, {compile, {cuter:mod(), term()}}}.
-type load_error()    :: code_error() | abs_code_error() | compile_error().

-type tagID() :: integer().
-opaque tag() :: {?BRANCH_TAG_PREFIX, tagID()}.
-type tag_generator() :: fun(() -> tag()).

-type lineno() :: integer().
-type name() :: atom().
-type cerl_attr() :: {#c_literal{val :: 'type'}, #c_literal{val :: cerl_attr_type()}}
                   | {#c_literal{val :: 'spec'}, #c_literal{val :: cerl_attr_spec()}}.
-type cerl_attr_type() :: cerl_recdef() | cerl_typedef().
-type cerl_attr_spec() :: cerl_specdef().

-type cerl_recdef() :: {{'record', name()}, [cerl_record_field()], []}.
-type cerl_record_field() :: cerl_untyped_record_field() | cerl_typed_record_field().
-type cerl_untyped_record_field() :: {'record_field', lineno(), name()}
                                   | {'record_field', lineno(), name(), any()}.
-type cerl_typed_record_field() :: {'typed_record_field', cerl_untyped_record_field(), cerl_type()}.
-type cerl_typedef() :: {name(), cerl_type(), [cerl:c_var()]}.

-type cerl_specdef() :: {name(), byte(), cerl_spec()}.
-type cerl_spec() :: [cerl_func() | cerl_bounded_func(), ...].

-type cerl_bounded_func() :: {'type', lineno(), 'bounded_fun', [cerl_func() | cerl_constraint()]}.
-type cerl_func() :: {'type', lineno(), 'fun', [cerl_product() | cerl_type()]}.
-type cerl_constraint() :: {'type', lineno(), 'constraint', [{atom, lineno(), 'is_subtype'} | [atom() | cerl_type()]]}.
-type cerl_product() :: {'type', lineno(), 'product', [cerl_type()]}.

-type cerl_type() :: cerl_type_nil()
                   | cerl_type_any()
                   | cerl_type_integer()
                   | cerl_type_float()
                   | cerl_type_boolean()
                   | cerl_type_atom()
                   | cerl_type_module()
                   | cerl_type_number()
                   | cerl_type_char()
                   | cerl_type_byte()
                   | cerl_type_binary()
                   | cerl_type_bitstring()
                   | cerl_type_string()
                   | cerl_type_tuple()
                   | cerl_type_literal()
                   | cerl_type_list()
                   | cerl_type_nonempty_list()
                   | cerl_type_union()
                   | cerl_type_range()
                   | cerl_type_function()
                   | cerl_type_ann()
                   | cerl_type_paren()
                   | cerl_type_remote()
                   | cerl_type_record()
                   | cerl_type_local()
                   | cerl_type_var()
                   .


-type cerl_type_nil() :: {'type', lineno(), 'nil', []}.
-type cerl_type_any() :: {'type', lineno(), 'any' | 'term', []}.
-type cerl_type_integer() :: {'type', lineno(), 'integer' | 'pos_integer' | 'non_neg_integer' | 'neg_integer', []}.
-type cerl_type_float() :: {'type', lineno(), 'float', []}.
-type cerl_type_boolean() :: {'type', lineno(), 'boolean', []}.
-type cerl_type_atom() :: {'type', lineno(), 'atom', []}.
-type cerl_type_module() :: {'type', lineno(), 'module', []}.
-type cerl_type_number() :: {'type', lineno(), 'number', []}.
-type cerl_type_char() :: {'type', lineno(), 'char', []}.
-type cerl_type_byte() :: {'type', lineno(), 'byte', []}.
-type cerl_type_binary() :: {'type', lineno(), 'binary', []}.
-type cerl_type_bitstring() :: {'type', lineno(), 'bitstring', []}.
-type cerl_type_string() :: {'type', lineno(), 'string', []}.
-type cerl_type_tuple() :: {'type', lineno(), 'tuple', 'any' | [cerl_type(), ...]}.
-type cerl_type_literal() :: cerl_type_literal_atom() | cerl_type_literal_integer().
-type cerl_type_literal_atom() :: {'atom', lineno(), atom()}.
-type cerl_type_literal_integer() :: {'integer', lineno(), integer()}.
-type cerl_type_list() :: {'type', lineno(), 'list', [cerl_type()]}.
-type cerl_type_nonempty_list() :: {'type', lineno(), 'nonempty_list', [cerl_type()]}.
-type cerl_type_union() :: {'type', lineno(), 'union', [cerl_type()]}.
-type cerl_type_range() :: {'type', lineno(), 'range', [cerl_type_literal_integer()]}.
-type cerl_type_ann() :: {'ann_type', lineno(), [cerl:c_var() | cerl_type()]}.
-type cerl_type_paren() :: {'paren_type', lineno(), cerl_type()}.
-type cerl_type_remote() :: {'remote_type', lineno(), [cerl_type_literal_atom() | [cerl_type()]]}.
-type cerl_type_record() :: {'type', lineno(), 'record', [cerl_type_literal_atom() | [cerl_type_record()]]}.
-type cerl_type_record_field() :: {'type', lineno(), 'field_type', [cerl_type_literal_atom() | cerl_type()]}.
-type cerl_type_local() :: {'type', lineno(), cerl_type_literal_atom(), [cerl_type()]}.
-type cerl_type_var() :: cerl:c_var().
-type cerl_type_function() :: {'type', lineno(), 'function', []}
                            | cerl_func() | cerl_bounded_func().


%%====================================================================
%% External exports
%%====================================================================
-spec load(M, ets:tid(), tag_generator()) -> {ok, M} | load_error() when M :: cuter:mod().
load(Mod, Db, TagGen) ->
  case get_core_ast(Mod) of
    {ok, AST} ->
      store_module(Mod, AST, Db, TagGen),
      {ok, Mod};
    {error, _} = Error -> Error
  end.

%% Retrieves the spec of a function from a stored module's info.
-spec retrieve_spec(ets:tid(), {name(), byte()}) -> cerl_spec() | not_found.
retrieve_spec(Db, FA) ->
  [{specs, SpecAttrs}] = ets:lookup(Db, specs),
  locate_spec(SpecAttrs, FA).

%% Locates the spec of a function from the list of the module's attributes.
-spec locate_spec([cerl_attr_spec()], {name(), byte()}) -> cerl_spec() | not_found.
locate_spec([], _FA) ->
  not_found;
locate_spec([{FA, Spec} | _Attrs], FA) ->
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
%% exported             [{M :: module(), Fun :: atom(), Arity :: arity()}]
%% attributes           Attrs :: [{cerl(), cerl()}]
%% {M, Fun, Arity}      {Def :: #c_fun{}, Exported :: boolean()}
-spec store_module(cuter:mod(), cerl:cerl(), ets:tid(), tag_generator()) -> ok.
store_module(M, AST, Db, TagGen) ->
  store_module_info(anno, M, AST, Db),
  store_module_info(name, M, AST, Db),
  store_module_info(exports, M, AST, Db),
  store_module_info(attributes, M, AST, Db),
  store_module_funs(M, AST, Db, TagGen),
  ok.

%% Gets the Core Erlang AST of a module.
-spec get_core_ast(cuter:mod()) -> {ok, cerl:cerl()} | load_error().
get_core_ast(M) ->
  case mod_beam_path(M) of
    {ok, BeamPath} ->
      case extract_abstract_code(M, BeamPath) of
	{ok, AbstractCode} ->
	  case compile:forms(AbstractCode, [to_core]) of
	    {ok, M, AST} -> {ok, AST};
	    {ok, M, AST, _Warns} -> {ok, AST};
	    Errors -> {error, {compile, {M, Errors}}}
	  end;
	{error, _} = Error -> Error
      end;
    {error, _} = Error -> Error
  end.
  
%% Gets the path of the module's beam file, if such one exists.
-spec mod_beam_path(cuter:mod()) -> {ok, file:filename()} | code_error().
mod_beam_path(M) ->
  case code:which(M) of
    non_existing   -> {error, {non_existing, M}};
    preloaded      -> {error, {preloaded, M}};
    cover_compiled -> {error, {cover_compiled, M}};
    Path -> {ok, Path}
  end.

%% Gets the abstract code from a module's beam file, if possible.
-spec extract_abstract_code(cuter:mod(), file:name()) -> {ok, list()} | abs_code_error().
extract_abstract_code(Mod, Beam) ->
  case beam_lib:chunks(Beam, [abstract_code]) of
    {error, beam_lib, _} ->
      {error, {missing_abstract_code, Mod}};
    {ok, {Mod, [{abstract_code, no_abstract_code}]}} ->
      {error, {missing_abstract_code, Mod}};
    {ok, {Mod, [{abstract_code, {_, AbstractCode}}]}} ->
      {ok, AbstractCode}
  end.

%% Store module information
-spec store_module_info(info(), cuter:mod(), cerl:c_module(), ets:tab()) -> ok.
store_module_info(anno, _M, AST, Db) ->
  Anno = AST#c_module.anno,
  true = ets:insert(Db, {anno, Anno}),
  ok;
store_module_info(attributes, _M, AST, Db) ->
  Attrs = cerl:module_attrs(AST),
  true = ets:insert(Db, {attributes, Attrs}),
  %% Retrieve the attributes that involve type & record declarations.
  {TypeAttrs, SpecAttrs} = classify_attributes(Attrs),
  %% Pre-process those declarations.
  Types = cuter_types:retrieve_types(TypeAttrs),
  true = ets:insert(Db, {types, Types}),
  %% Just store the attributes that involve specs.
  true = ets:insert(Db, {specs, SpecAttrs}),
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

%% Store exported functions of a module
-spec store_module_funs(cuter:mod(), cerl:cerl(), ets:tab(), tag_generator()) -> ok.
store_module_funs(M, AST, Db, TagGen) ->
  Funs = AST#c_module.defs,
  [{exported, Exps}] = ets:lookup(Db, exported),
  lists:foreach(fun(X) -> store_fun(Exps, M, X, Db, TagGen) end, Funs).

%% Store the AST of a function
-spec store_fun([atom()], cuter:mod(), {cerl:c_var(), cerl:c_fun()}, ets:tab(), tag_generator()) -> ok.
store_fun(Exps, M, {Fun, Def}, Db, TagGen) ->
  {FunName, Arity} = Fun#c_var.name,
  MFA = {M, FunName, Arity},
  Exported = lists:member(MFA, Exps),
%  io:format("===========================================================================~n"),
%  io:format("BEFORE~n"),
%  io:format("~p~n", [Def]),
  AnnDef = annotate(Def, TagGen),
%  io:format("AFTER~n"),
%  io:format("~p~n", [AnnDef]),
  true = ets:insert(Db, {MFA, {AnnDef, Exported}}),
  ok.

-spec classify_attributes([cerl_attr()]) -> {[cerl_attr_type()], [cerl_attr_spec()]}.
classify_attributes(Attrs) ->
  classify_attributes(Attrs, [], []).

-spec classify_attributes([cerl_attr()], [cerl_attr_type()], [cerl_attr_spec()]) -> {[cerl_attr_type()], [cerl_attr_spec()]}.
classify_attributes([], Types, Specs) ->
  {lists:reverse(Types), lists:reverse(Specs)};
classify_attributes([{What, #c_literal{val = [Info]}}|Attrs], Types, Specs) ->
  case cerl:atom_val(What) of
    type -> classify_attributes(Attrs, [Info|Types], Specs);
    spec -> classify_attributes(Attrs, Types, [Info|Specs])
  end.

%% Annotates the AST with tags.
-spec annotate(cerl:cerl(), tag_generator()) -> cerl:cerl().
annotate(Def, TagGen) -> annotate_pats(Def, TagGen, false).

%% Annotate patterns
-spec annotate_pats(cerl:cerl(), tag_generator(), boolean()) -> cerl:cerl().
annotate_pats({c_alias, Anno, Var, Pat}, TagGen, InPats) ->
  {c_alias, Anno, annotate_pats(Var, TagGen, InPats), annotate_pats(Pat, TagGen, InPats)};
annotate_pats({c_apply, Anno, Op, Args}, TagGen, InPats) ->
  {c_apply, [TagGen()|Anno], annotate_pats(Op, TagGen, InPats), [annotate_pats(A, TagGen, InPats) || A <- Args]};
annotate_pats({c_binary, Anno, Segs}, TagGen, InPats) ->
  {c_binary, Anno, [annotate_pats(S, TagGen, InPats) || S <- Segs]};
annotate_pats({c_bitstr, Anno, Val, Sz, Unit, Type, Flags}, TagGen, InPats) ->
  {c_bitstr, Anno, annotate_pats(Val, TagGen, InPats), annotate_pats(Sz, TagGen, InPats),
    annotate_pats(Unit, TagGen, InPats), annotate_pats(Type, TagGen, InPats), annotate_pats(Flags, TagGen, InPats)};
annotate_pats({c_call, Anno, Mod, Name, Args}, TagGen, InPats) ->
  {c_call, [TagGen()|Anno], annotate_pats(Mod, TagGen, InPats), annotate_pats(Name, TagGen, InPats), [annotate_pats(A, TagGen, InPats) || A <- Args]};
annotate_pats({c_case, Anno, Arg, Clauses}, TagGen, InPats) ->
  {c_case, Anno, annotate_pats(Arg, TagGen, InPats), [annotate_pats(Cl, TagGen, InPats) || Cl <- Clauses]};
annotate_pats({c_catch, Anno, Body}, TagGen, InPats) ->
  {c_catch, Anno, annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_clause, Anno, Pats, Guard, Body}, TagGen, InPats) ->
  WithTags = [{next_tag, TagGen()}, TagGen() | Anno],
  {c_clause, WithTags, [annotate_pats(P, TagGen, true) || P <- Pats], annotate_pats(Guard, TagGen, InPats), annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_cons, Anno, Hd, Tl}, TagGen, InPats) ->
  WithTags = [{next_tag, TagGen()}, TagGen() | Anno],
  case InPats of
    false -> {c_cons, Anno, annotate_pats(Hd, TagGen, false), annotate_pats(Tl, TagGen, false)};
    true  -> {c_cons, WithTags, annotate_pats(Hd, TagGen, true), annotate_pats(Tl, TagGen, true)}
  end;
annotate_pats({c_fun, Anno, Vars, Body}, TagGen, InPats) ->
  {c_fun, Anno, [annotate_pats(V, TagGen, InPats) || V <- Vars], annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_let, Anno, Vars, Arg, Body}, TagGen, InPats) ->
  {c_let, Anno, [annotate_pats(V, TagGen, InPats) || V <- Vars], annotate_pats(Arg, TagGen, InPats), annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_letrec, Anno, Defs, Body}, TagGen, InPats) ->
  {c_letrec, Anno, [{annotate_pats(X, TagGen, InPats), annotate_pats(Y, TagGen, InPats)} || {X, Y} <- Defs], annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_literal, Anno, Val}, TagGen, InPats) ->
  case InPats of
    false -> {c_literal, Anno, Val};
    true  -> {c_literal, [{next_tag, TagGen()}, TagGen() | Anno], Val}
  end;
annotate_pats({c_primop, Anno, Name, Args}, TagGen, InPats) ->
  {c_primop, Anno, annotate_pats(Name, TagGen, InPats), [annotate_pats(A, TagGen, InPats) || A <- Args]};
annotate_pats({c_receive, Anno, Clauses, Timeout, Action}, TagGen, InPats) ->
  {c_receive, Anno, [annotate_pats(Cl, TagGen, InPats) || Cl <- Clauses], annotate_pats(Timeout, TagGen, InPats), annotate_pats(Action, TagGen, InPats)};
annotate_pats({c_seq, Anno, Arg, Body}, TagGen, InPats) ->
  {c_seq, Anno, annotate_pats(Arg, TagGen, InPats), annotate_pats(Body, TagGen, InPats)};
annotate_pats({c_try, Anno, Arg, Vars, Body, Evars, Handler}, TagGen, InPats) ->
  {c_try, Anno, annotate_pats(Arg, TagGen, InPats), [annotate_pats(V, TagGen, InPats) || V <- Vars],
    annotate_pats(Body, TagGen, InPats), [annotate_pats(EV, TagGen, InPats) || EV <- Evars], annotate_pats(Handler, TagGen, InPats)};
annotate_pats({c_tuple, Anno, Es}, TagGen, InPats) ->
  WithTags = [{next_tag, TagGen()}, TagGen() | Anno],
  case InPats of
    false -> {c_tuple, Anno, [annotate_pats(E, TagGen, false) || E <- Es]};
    true  -> {c_tuple, WithTags, [annotate_pats(E, TagGen, true) || E <- Es]}
  end;
annotate_pats({c_values, Anno, Es}, TagGen, InPats) ->
  {c_values, Anno, [annotate_pats(E, TagGen, InPats) || E <- Es]};
annotate_pats({c_var, Anno, Name}, _TagGen, _InPats) ->
  {c_var, Anno, Name};
annotate_pats(X, _,_) -> X. %% FIXME temp fix for maps

%% Get the tags from the annotations of an AST's node.
-spec get_tags(list()) -> ast_tags().
get_tags(Annos) ->
  get_tags(Annos, #tags{}).

get_tags([], Tags) ->
  Tags;
get_tags([{?BRANCH_TAG_PREFIX, _N}=Tag | Annos], Tags) ->
  get_tags(Annos, Tags#tags{this = Tag});
get_tags([{next_tag, Tag={?BRANCH_TAG_PREFIX, _N}} | Annos], Tags) ->
  get_tags(Annos, Tags#tags{next = Tag});
get_tags([_|Annos], Tags) ->
  get_tags(Annos, Tags).

%% Creates a tag from tag info.
-spec tag_from_id(tagID()) -> tag().
tag_from_id(N) ->
  {?BRANCH_TAG_PREFIX, N}.

%% Gets the tag info of a tag.
-spec id_of_tag(tag()) -> tagID().
id_of_tag({?BRANCH_TAG_PREFIX, N}) -> N.

%% Creates an empty tag.
-spec empty_tag() -> tag().
empty_tag() ->
  tag_from_id(?EMPTY_TAG_ID).
