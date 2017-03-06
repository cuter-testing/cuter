%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl).

%% External exports
-export([retrieve_spec/2, get_tags/1, id_of_tag/1, tag_from_id/1, empty_tag/0,
         get_stored_types/1, empty_tagId/0, collect_feasible_tags/2]).
%% Core AST extraction.
-export([load/4, get_core/2]).
%% Node types generators.
-export([node_types_branches/0, node_types_branches_nocomp/0, node_types_all/0,
         node_types_conditions/0, node_types_conditions_nocomp/0,
         node_types_paths/0, node_types_paths_nocomp/0]).
%% Exported for debugging use.
-export([classify_attributes/1]).

%% We are using the records representation of Core Erlang Abstract Syntax Trees
-include_lib("compiler/src/core_parse.hrl").

-include("include/cuter_macros.hrl").

-export_type([compile_error/0, cerl_attr_spec/0, cerl_attr_type/0,
	      cerl_bounded_func/0, cerl_constraint/0, cerl_func/0,
	      cerl_recdef/0, cerl_record_field/0, cerl_spec/0,
	      cerl_spec_func/0, cerl_type/0, cerl_typedef/0,
	      cerl_type_record_field/0, node_types/0,
	      tagID/0, tag/0, tag_generator/0, visited_tags/0,
	      type_info/0, spec_info/0]).

-type info()          :: 'anno' | 'attributes' | 'exports' | 'name'.
-type compile_error() :: {'error', string()}.
-type load_error()    :: {'error', 'preloaded'} | compile_error().

-type tagID() :: integer().
-opaque tag() :: {?BRANCH_TAG_PREFIX, tagID()}.
-type tag_generator() :: fun(() -> tag()).
-type visited_tags() :: gb_sets:set(tagID()).
-type node_types() :: {(conditions | paths | branches), boolean()} | all.

-type lineno() :: integer().
-type name() :: atom().
-type fa() :: {name(), arity()}.
-type cerl_attr() :: {#c_literal{val :: 'type' | opaque}, #c_literal{val :: cerl_attr_type()}}
                   | {#c_literal{val :: 'record'}, #c_literal{val :: cerl_recdef()}} % for OTP 19.x
                   | {#c_literal{val :: 'spec'}, #c_literal{val :: cerl_attr_spec()}}
                   | {#c_literal{val :: 'export_type' | 'behaviour'}, cerl:c_literal()}.
-type cerl_attr_type() :: cerl_recdef() | cerl_typedef().
-type cerl_attr_spec() :: cerl_specdef().

-type cerl_recdef() :: {name(), [cerl_record_field()]} % for OTP 19.x
                     | {{'record', name()}, [cerl_record_field()], []}. % for OTP 18.x or earlier
-type cerl_record_field() :: cerl_untyped_record_field() | cerl_typed_record_field().
-type cerl_untyped_record_field() :: {'record_field', lineno(), {'atom', lineno(), name()}}
                                   | {'record_field', lineno(), {'atom', lineno(), name()}, any()}.
-type cerl_typed_record_field() :: {'typed_record_field', cerl_untyped_record_field(), cerl_type()}.
-type cerl_typedef() :: {name(), cerl_type(), [cerl_type_var()]}.

-type cerl_specdef() :: {fa(), cerl_spec()}.
-type cerl_spec() :: [cerl_spec_func(), ...].
-type cerl_spec_func() :: cerl_func() | cerl_bounded_func().

-type cerl_bounded_func() :: {'type', lineno(), 'bounded_fun', [cerl_func() | cerl_constraint()]}.
-type cerl_func() :: {'type', lineno(), 'fun', [cerl_product() | cerl_type()]}.
-type cerl_constraint() :: {'type', lineno(), 'constraint', [{'atom', lineno(), 'is_subtype'} | [cerl_type_var() | cerl_type()]]}.
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
                   | cerl_type_bitstringMN()
                   | cerl_type_string()
                   | cerl_type_tuple()
                   | cerl_type_literal()
                   | cerl_type_list()
                   | cerl_type_nonempty_list()
                   | cerl_type_union()
                   | cerl_type_range()
                   | cerl_type_function()
                   | cerl_type_map()
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
-type cerl_type_bitstringMN() :: {'type', lineno(), 'bitstring', [{integer, lineno(), non_neg_integer()}]}.
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
-type cerl_type_local() :: {'type' | 'user_type', lineno(), cerl_type_literal_atom(), [cerl_type()]}.
-type cerl_type_map() :: {'type', lineno(), 'map', any()}.  %% TODO Refine map representation.
-type cerl_type_var() :: {'var', lineno(), atom()}.
-type cerl_type_function() :: {'type', lineno(), 'function', []}
                            | cerl_func() | cerl_bounded_func().


%%====================================================================
%% External exports
%%====================================================================
-spec load(M, cuter_codeserver:module_cache(), tag_generator(), boolean()) -> {ok, M} | load_error() when M :: cuter:mod().
load(Mod, Cache, TagGen, WithPmatch) ->
  case get_core(Mod, WithPmatch) of
    {ok, #c_module{}=AST} -> % just a sanity check that we get back a module
      store_module(Mod, AST, Cache, TagGen),
      {ok, Mod};
    {error, _} = Error -> Error
  end.

%% Retrieves the spec of a function from a stored module's info.
-spec retrieve_spec(cuter_codeserver:module_cache(), fa()) -> {ok, cuter_types:stored_spec_value()} | error.
retrieve_spec(Cache, FA) ->
  {ok, Specs} = cuter_codeserver:lookup_in_module_cache(specs, Cache),
  cuter_types:find_spec(FA, Specs).

-spec get_stored_types(cuter_codeserver:module_cache()) -> cuter_types:stored_types().
get_stored_types(Cache) ->
  {ok, Types} = cuter_codeserver:lookup_in_module_cache(types, Cache),
  Types.

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
-spec store_module(cuter:mod(), cerl:cerl(), cuter_codeserver:module_cache(), tag_generator()) -> ok.
store_module(M, AST, Cache, TagGen) ->
  store_module_info(anno, M, AST, Cache),
  store_module_info(name, M, AST, Cache),
  store_module_info(exports, M, AST, Cache),
  store_module_info(attributes, M, AST, Cache),
  store_module_funs(M, AST, Cache, TagGen).

%% Gets the Core Erlang AST of a module.
-spec get_core(cuter:mod(), boolean()) -> {ok, cerl:cerl()} | load_error().
get_core(M, WithPmatch) ->
  try
    case beam_path(M) of
      {ok, BeamPath} ->
        AbstractCode = get_abstract_code(M, BeamPath),
        case compile:forms(AbstractCode, compile_options(WithPmatch)) of
          {ok, M, AST} -> {ok, AST};
          {ok, M, AST, _Warns} -> {ok, AST};
          Errors -> {error, cuter_pp:compilation_errors(M, Errors)}
        end;
      preloaded -> {error, preloaded}
    end
  catch
    throw:Reason -> {error, Reason}
  end.

%% The compilation options.
compile_options(true) -> [to_core, {core_transform, cerl_pmatch}];
compile_options(false) -> [to_core].

%% Gets the path of the module's beam file, if such one exists.
-spec beam_path(cuter:mod()) -> {ok, file:filename()} | preloaded.
beam_path(M) ->
  case code:which(M) of
    preloaded      -> preloaded;
    non_existing   -> throw(cuter_pp:non_existing_module(M));
    cover_compiled -> throw(cuter_pp:cover_compiled_module(M));
    Path -> {ok, Path}
  end.

%% Gets the abstract code from a module's beam file, if possible.
-spec get_abstract_code(cuter:mod(), file:name()) -> [erl_parse:abstract_form()].
get_abstract_code(Mod, Beam) ->
  case beam_lib:chunks(Beam, [abstract_code]) of
    {ok, {Mod, [{abstract_code, {_, AbstractCode}}]}} -> AbstractCode;
    _ -> throw(cuter_pp:abstract_code_missing(Mod))
  end.

%% Store module information
-spec store_module_info(info(), cuter:mod(), cerl:c_module(), cuter_codeserver:module_cache()) -> ok.
store_module_info(anno, _M, AST, Cache) ->
  Anno = AST#c_module.anno,
  cuter_codeserver:insert_in_module_cache(anno, Anno, Cache);
store_module_info(attributes, _M, AST, Cache) ->
  Attrs = cerl:module_attrs(AST),
  cuter_codeserver:insert_in_module_cache(attributes, Attrs, Cache),
  %% Retrieve the attributes that involve type & record declarations.
  {TypeAttrs, SpecAttrs} = classify_attributes(Attrs),
  %% Pre-process those declarations.
  Types = cuter_types:retrieve_types(TypeAttrs),
  cuter_codeserver:insert_in_module_cache(types, Types, Cache),
  %% Just store the attributes that involve specs.
  Specs = cuter_types:retrieve_specs(SpecAttrs),
  cuter_codeserver:insert_in_module_cache(specs, Specs, Cache);
store_module_info(exports, M, AST, Cache) ->
  Exps_c = AST#c_module.exports,
  Fun_info = 
    fun(Elem) ->
      {Fun, Arity} = Elem#c_var.name,
      {M, Fun, Arity}
    end,
  Exps = [Fun_info(E) || E <- Exps_c],
  cuter_codeserver:insert_in_module_cache(exported, Exps, Cache);
store_module_info(name, _M, AST, Cache) ->
  ModName_c = AST#c_module.name,
  ModName = ModName_c#c_literal.val,
  cuter_codeserver:insert_in_module_cache(name, ModName, Cache).

%% Store exported functions of a module
-spec store_module_funs(cuter:mod(), cerl:cerl(), cuter_codeserver:module_cache(), tag_generator()) -> ok.
store_module_funs(M, AST, Cache, TagGen) ->
  Funs = AST#c_module.defs,
  {ok, Exps} = cuter_codeserver:lookup_in_module_cache(exported, Cache),
  lists:foreach(fun(X) -> store_fun(Exps, M, X, Cache, TagGen) end, Funs).

%% Store the AST of a function
-spec store_fun([atom()], cuter:mod(), {cerl:c_var(), cerl:c_fun()}, cuter_codeserver:module_cache(), tag_generator()) -> ok.
store_fun(Exps, M, {Fun, Def}, Cache, TagGen) ->
  {FunName, Arity} = Fun#c_var.name,
  MFA = {M, FunName, Arity},
  Exported = lists:member(MFA, Exps),
%  io:format("===========================================================================~n"),
%  io:format("BEFORE~n"),
%  io:format("~p~n", [Def]),
  AnnDef = annotate(Def, TagGen),
%  io:format("AFTER~n"),
%  io:format("~p~n", [AnnDef]),
  cuter_codeserver:insert_in_module_cache(MFA, {AnnDef, Exported}, Cache).

-type type_info() :: {'type', cerl_typedef()}
                   | {'record', cerl_recdef()}.
-type spec_info() :: cerl_attr_spec().
-type classify_attr_ret() :: {[type_info()], [spec_info()]}.

-spec classify_attributes([cerl_attr()]) -> classify_attr_ret().
classify_attributes(Attrs) ->
  classify_attributes(Attrs, [], []).

-spec classify_attributes([cerl_attr()], [type_info()], [spec_info()]) -> classify_attr_ret().
classify_attributes([], Types, Specs) ->
  {lists:reverse(Types), lists:reverse(Specs)};
classify_attributes([{What, #c_literal{val = Val}}|Attrs], Types, Specs) ->
  case cerl:atom_val(What) of
    Tp when Tp =:= type orelse Tp =:= opaque ->
      [V] = Val,
      case V of
        {{record, Name}, Fields, []} -> % for OTP 18.x and earlier
          classify_attributes(Attrs, [{record, {Name, Fields}}|Types], Specs);
        _ ->
          classify_attributes(Attrs, [{type, V}|Types], Specs)
      end;
    record -> % for OTP 19.x and newer
      [V] = Val,
      classify_attributes(Attrs, [{record, V}|Types], Specs);
    spec ->
      [V] = Val,
      classify_attributes(Attrs, Types, [V|Specs]);
    _Ignore ->
      classify_attributes(Attrs, Types, Specs)
  end.

%% ----------------------------------------------------------------------------
%% Annotate the AST with tags.
%% ----------------------------------------------------------------------------

-spec annotate(cerl:cerl(), tag_generator()) -> cerl:cerl().
annotate(Def, TagGen) -> annotate(Def, TagGen, false).

-spec annotate(cerl:cerl(), tag_generator(), boolean()) -> cerl:cerl().
annotate(Tree, TagGen, InPats) ->
  case cerl:type(Tree) of
    alias ->
      Var = annotate(cerl:alias_var(Tree), TagGen, InPats),
      Pat = annotate(cerl:alias_pat(Tree), TagGen, InPats),
      cerl:update_c_alias(Tree, Var, Pat);
    'apply' ->
      % TODO Annotate applications for lambda terms.
      Op = annotate(cerl:apply_op(Tree), TagGen, InPats),
      Args = annotate_all(cerl:apply_args(Tree), TagGen, InPats),
      cerl:update_c_apply(Tree, Op, Args);
    binary ->
      Segs = annotate_all(cerl:binary_segments(Tree), TagGen, InPats),
      T = cerl:update_c_binary(Tree, Segs),
      case InPats of
        false -> T;
        true  -> cerl:add_ann(tag_pair(TagGen), T)
      end;
    bitstr ->
      Val = annotate(cerl:bitstr_val(Tree), TagGen, InPats),
      Size = annotate(cerl:bitstr_size(Tree), TagGen, InPats),
      Unit = annotate(cerl:bitstr_unit(Tree), TagGen, InPats),
      Type = annotate(cerl:bitstr_type(Tree), TagGen, InPats),
      Flags = annotate(cerl:bitstr_flags(Tree), TagGen, InPats),
      T = cerl:update_c_bitstr(Tree, Val, Size, Unit, Type, Flags),
      case InPats of
        false -> T;
        true  -> cerl:add_ann(tag_pair(TagGen), T)
      end;
    call ->
      Mod = annotate(cerl:call_module(Tree), TagGen, InPats),
      Name = annotate(cerl:call_name(Tree), TagGen, InPats),
      Args = annotate_all(cerl:call_args(Tree), TagGen, InPats),
      cerl:update_c_call(Tree, Mod, Name, Args);
    'case' ->
      Arg = annotate(cerl:case_arg(Tree), TagGen, InPats),
      Clauses0 = annotate_all(cerl:case_clauses(Tree), TagGen, InPats),
      Clauses = mark_last_clause(Clauses0),
      cerl:update_c_case(Tree, Arg, Clauses);
    'catch' ->
      Body = annotate(cerl:catch_body(Tree), TagGen, InPats),
      cerl:update_c_catch(Tree, Body);
    clause ->
      Pats = annotate_all(cerl:clause_pats(Tree), TagGen, true),
      Guard = annotate(cerl:clause_guard(Tree), TagGen, InPats),
      Body = annotate(cerl:clause_body(Tree), TagGen, InPats),
      T = cerl:update_c_clause(Tree, Pats, Guard, Body),
      cerl:add_ann(tag_pair(TagGen), T);
    cons ->
      Hd = annotate(cerl:cons_hd(Tree), TagGen, InPats),
      Tl = annotate(cerl:cons_tl(Tree), TagGen, InPats),
      T = cerl:update_c_cons_skel(Tree, Hd, Tl),
      case InPats of
        false -> T;
        true  -> cerl:add_ann(tag_pair(TagGen), T)
      end;
    'fun' ->
      Vars = annotate_all(cerl:fun_vars(Tree), TagGen, InPats),
      Body = annotate(cerl:fun_body(Tree), TagGen, InPats),
      cerl:update_c_fun(Tree, Vars, Body);
    'let' ->
      Vars = annotate_all(cerl:let_vars(Tree), TagGen, InPats),
      Arg = annotate(cerl:let_arg(Tree), TagGen, InPats),
      Body = annotate(cerl:let_body(Tree), TagGen, InPats),
      cerl:update_c_let(Tree, Vars, Arg, Body);
    letrec ->
      Combine = fun(X, Y) -> {annotate(X, TagGen, InPats), annotate(Y, TagGen, InPats)} end,
      Defs = [Combine(N, D) || {N, D} <- cerl:letrec_defs(Tree)],
      Body = annotate(cerl:letrec_body(Tree), TagGen, InPats),
      cerl:update_c_letrec(Tree, Defs, Body);
    literal ->
      case InPats of
        false -> Tree;
        true  -> cerl:add_ann(tag_pair(TagGen), Tree)
      end;
    primop ->
      Name = annotate(cerl:primop_name(Tree), TagGen, InPats),
      Args = annotate_all(cerl:primop_args(Tree), TagGen, InPats),
      cerl:update_c_primop(Tree, Name, Args);
    'receive' ->
      Clauses0 = annotate_all(cerl:receive_clauses(Tree), TagGen, InPats),
      Clauses = mark_last_clause(Clauses0),
      Timeout = annotate(cerl:receive_timeout(Tree), TagGen, InPats),
      Action = annotate(cerl:receive_action(Tree), TagGen, InPats),
      cerl:update_c_receive(Tree, Clauses, Timeout, Action);
    seq ->
      Arg = annotate(cerl:seq_arg(Tree), TagGen, InPats),
      Body = annotate(cerl:seq_body(Tree), TagGen, InPats),
      cerl:update_c_seq(Tree, Arg, Body);
    'try' ->
      Arg = annotate(cerl:try_arg(Tree), TagGen, InPats),
      Vars = annotate_all(cerl:try_vars(Tree), TagGen, InPats),
      Body = annotate(cerl:try_body(Tree), TagGen, InPats),
      Evars = annotate_all(cerl:try_evars(Tree), TagGen, InPats),
      Handler = annotate(cerl:try_handler(Tree), TagGen, InPats),
      cerl:update_c_try(Tree, Arg, Vars, Body, Evars, Handler);
    tuple ->
      Es = annotate_all(cerl:tuple_es(Tree), TagGen, InPats),
      T = cerl:update_c_tuple_skel(Tree, Es),
      case InPats of
        false -> T;
        true  -> cerl:add_ann(tag_pair(TagGen), T)
      end;
    values ->
      Es = annotate_all(cerl:values_es(Tree), TagGen, InPats),
      cerl:update_c_values(Tree, Es);
    var ->
      Tree;
    _ ->
      Tree  %% TODO Ignore maps (for now) and modules.
  end.

annotate_all(Trees, TagGen, InPats) ->
  [annotate(T, TagGen, InPats) || T <- Trees].

mark_last_clause([]) ->
  [];
mark_last_clause(Clauses) when is_list(Clauses) ->
  [Last|Rvs] = lists:reverse(Clauses),
  AnnLast = cerl:add_ann([last_clause], Last),
  lists:reverse([AnnLast|Rvs]).

%% ----------------------------------------------------------------------------
%% Collect tags from AST for a specific mfa.
%% ----------------------------------------------------------------------------

-spec collect_feasible_tags(cerl:cerl(), node_types()) -> visited_tags().
collect_feasible_tags(Tree, NodeTypes) ->
  TagIDs = lists:flatten(collect(Tree, NodeTypes)),
  gb_sets:from_list(TagIDs).

collect(Tree, NodeTypes) ->
  case cerl:type(Tree) of
    alias ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:alias_var(Tree), NodeTypes)
      , collect(cerl:alias_pat(Tree), NodeTypes)
      ];
    'apply' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:apply_op(Tree), NodeTypes)
      , collect_all(cerl:apply_args(Tree), NodeTypes)
      ];
    binary ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:binary_segments(Tree), NodeTypes)
      ];
    bitstr ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:bitstr_val(Tree), NodeTypes)
      , collect(cerl:bitstr_size(Tree), NodeTypes)
      , collect(cerl:bitstr_unit(Tree), NodeTypes)
      , collect(cerl:bitstr_type(Tree), NodeTypes)
      , collect(cerl:bitstr_flags(Tree), NodeTypes)
      ];
    call ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:call_module(Tree), NodeTypes)
      , collect(cerl:call_name(Tree), NodeTypes)
      , collect_all(cerl:call_args(Tree), NodeTypes)
      ];
    'case' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:case_arg(Tree), NodeTypes)
      , collect_all(cerl:case_clauses(Tree), NodeTypes)
      ];
    'catch' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:catch_body(Tree), NodeTypes)
      ];
    clause ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:clause_pats(Tree), NodeTypes)
      , collect(cerl:clause_guard(Tree), NodeTypes)
      , collect(cerl:clause_body(Tree), NodeTypes)
      ];
    cons ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:cons_hd(Tree), NodeTypes)
      , collect(cerl:cons_tl(Tree), NodeTypes)
      ];
    'fun' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:fun_vars(Tree), NodeTypes)
      , collect(cerl:fun_body(Tree), NodeTypes)
      ];
    'let' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:let_vars(Tree), NodeTypes)
      , collect(cerl:let_arg(Tree), NodeTypes)
      , collect(cerl:let_body(Tree), NodeTypes)
      ];
    letrec ->
      [ collect_tagIDs(Tree, NodeTypes)
      , [[collect(N, NodeTypes), collect(D, NodeTypes)] || {N, D} <- cerl:letrec_defs(Tree)]
      , collect(cerl:letrec_body(Tree), NodeTypes)
      ];
    literal ->
      collect_tagIDs(Tree, NodeTypes);
    primop ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:primop_name(Tree), NodeTypes)
      , collect_all(cerl:primop_args(Tree), NodeTypes)
      ];
    'receive' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:receive_clauses(Tree), NodeTypes)
      , collect(cerl:receive_timeout(Tree), NodeTypes)
      , collect(cerl:receive_action(Tree), NodeTypes)
      ];
    seq ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:seq_arg(Tree), NodeTypes)
      , collect(cerl:seq_body(Tree), NodeTypes)
      ];
    'try' ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect(cerl:try_arg(Tree), NodeTypes)
      , collect_all(cerl:try_vars(Tree), NodeTypes)
      , collect(cerl:try_body(Tree), NodeTypes)
      , collect_all(cerl:try_evars(Tree), NodeTypes)
      , collect(cerl:try_handler(Tree), NodeTypes)
      ];
    tuple ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:tuple_es(Tree), NodeTypes)
      ];
    values ->
      [ collect_tagIDs(Tree, NodeTypes)
      , collect_all(cerl:values_es(Tree), NodeTypes)
      ];
    var ->
      collect_tagIDs(Tree, NodeTypes);
    _ ->
      []  %% TODO Ignore maps (for now) and modules.
  end.

collect_all(Trees, NodeTypes) ->
  [collect(T, NodeTypes) || T <- Trees].

-spec node_types_all() -> all.
node_types_all() -> all.

-spec node_types_conditions() -> {conditions, true}.
node_types_conditions() -> {conditions, true}.

-spec node_types_conditions_nocomp() -> {conditions, false}.
node_types_conditions_nocomp() -> {conditions, false}.

-spec node_types_paths() -> {paths, true}.
node_types_paths() -> {paths, true}.

-spec node_types_paths_nocomp() -> {paths, false}.
node_types_paths_nocomp() -> {paths, false}.

-spec node_types_branches() -> {branches, true}.
node_types_branches() -> {branches, true}.

-spec node_types_branches_nocomp() -> {branches, false}.
node_types_branches_nocomp() -> {branches, false}.

%% ----------------------------------------------------------------------------
%% Manage tags.
%% ----------------------------------------------------------------------------

%% Collect all the tags from an AST node.
-spec collect_tagIDs(cerl:cerl(), node_types()) -> [tagID()].
collect_tagIDs(Tree, NodeTypes) ->
  case NodeTypes of
    all ->
      Ann = cerl:get_ann(Tree),
      collect_tagIDs_h(Ann, {true, true}, []);
    {Tp, true} when Tp =:= conditions; Tp =:= paths ->
      SwitchFalse = (Tp =:= paths),
      Ann = cerl:get_ann(Tree),
      case has_true_guard(Tree) of
        true  -> [];
        false -> collect_tagIDs_h(Ann, {true, SwitchFalse}, [])
      end;
    {Tp, false} when Tp =:= conditions; Tp =:= paths ->
      SwitchFalse = (Tp =:= paths),
      Ann = cerl:get_ann(Tree),
      case has_true_guard(Tree) of
        true -> [];
        false ->
          case lists:member(compiler_generated, Ann) of
            true  -> [];
            false -> collect_tagIDs_h(Ann, {true, SwitchFalse}, [])
          end
      end;
    {branches, true} ->
      Ann = cerl:get_ann(Tree),
      case cerl:type(Tree) of
        clause -> collect_tagIDs_h(Ann, {true, false}, []);
        _ -> []
      end;
    {branches, false} ->
      Ann = cerl:get_ann(Tree),
      case cerl:type(Tree) =:= clause andalso not lists:member(compiler_generated, Ann) of
        false -> [];
        true -> collect_tagIDs_h(Ann, {true, false}, [])
      end
  end.

has_true_guard(Tree) ->
  case cerl:type(Tree) of
    clause ->
      Guard = cerl:clause_guard(Tree),
      cerl:is_literal(Guard) andalso cerl:is_literal_term(Guard)
        andalso cerl:concrete(Guard) =:= true;
    _ ->
      false
  end.

collect_tagIDs_h([], _, Acc) ->
  Acc;
collect_tagIDs_h([{?BRANCH_TAG_PREFIX, N} | Rest], {true, _}=Switch, Acc) ->
  collect_tagIDs_h(Rest, Switch, [N|Acc]);
collect_tagIDs_h([{next_tag, {?BRANCH_TAG_PREFIX, N}} | Rest], {_, true}=Switch, Acc) ->
  collect_tagIDs_h(Rest, Switch, [N|Acc]);
collect_tagIDs_h([_|Rest], Switch, Acc) ->
  collect_tagIDs_h(Rest, Switch, Acc).

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

%% Generates a pair of tags.
tag_pair(TagGen) ->
  [{next_tag, TagGen()}, TagGen()].

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

%% Returns the empty tag id.
-spec empty_tagId() -> ?EMPTY_TAG_ID.
empty_tagId() -> ?EMPTY_TAG_ID.
