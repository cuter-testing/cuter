%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_cerl).

%% External exports
-export([get_tags/1, id_of_tag/1, tag_from_id/1, empty_tag/0,
         empty_tagId/0, collect_feasible_tags/2]).
%% Core AST extraction.
-export([load/3, get_core/2]).
%% Node types generators.
-export([node_types_branches/0, node_types_branches_nocomp/0, node_types_all/0,
         node_types_conditions/0, node_types_conditions_nocomp/0,
         node_types_paths/0, node_types_paths_nocomp/0]).
%% kfun API.
-export([kfun/2, kfun_code/1, kfun_is_exported/1]).
%% kmodule API.
-export([kmodule_spec_forms/1, kmodule_record_forms/1, kmodule_type_forms/1, kmodule_exported_types/1, kmodule_name/1, destroy_kmodule/1, kmodule/3, kmodule_kfun/2, kmodule_mfa_spec/2,
  kmodule_specs/1, kmodule_types/1, kmodule_update_kfun/3, kmodule_mfas_with_kfuns/1,
  kmodule_mfas_with_spec_forms/1]).
%% Export for unit tests.
-export([extract_record_forms/1, extract_type_forms/1]).

%% We are using the records representation of Core Erlang Abstract Syntax Trees
-include_lib("compiler/src/core_parse.hrl").

-include("include/cuter_macros.hrl").

-export_type([compile_error/0, cerl_spec_form/0, cerl_attr_type/0,
	      cerl_bounded_func/0, cerl_constraint/0, cerl_func/0,
	      cerl_record_field/0, cerl_spec/0,
	      cerl_spec_func/0, cerl_type/0, cerl_typedef/0,
	      cerl_type_record_field/0, node_types/0,
	      tagID/0, tag/0, tag_generator/0, visited_tags/0]).

-export_type([extracted_record_form/0, extracted_type_form/0]).

-export_type([kfun/0, kmodule/0]).

-type ast() :: cerl:c_module().

%% TODO(aggelgian): Change it to a map.
%% In each kmodule(), the following information is stored:
%%   Key               Value
%% --------  ---------------------------
%% types      cuter_types:stored_types()
%% specs      cuter_types:stored_specs()
%% mfa()      kfun()
-opaque kmodule() :: ets:tid().

%% kfun() is an ADT that holds the code and metadata for an MFA.
-opaque kfun() :: #{
  %% The code of the MFA.
  code := code(),
  %% Whether the MFA is exported or not.
  is_exported := boolean()
}.
-type code() :: cerl:c_fun().

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
-type cerl_attr_type() :: cerl_recdef() | cerl_typedef().

-type cerl_recdef() :: {name(), [cerl_record_field()]}.
-type cerl_record_field() :: cerl_untyped_record_field() | cerl_typed_record_field().
-type cerl_untyped_record_field() :: {'record_field', lineno(), {'atom', lineno(), name()}}
                                   | {'record_field', lineno(), {'atom', lineno(), name()}, any()}.
-type cerl_typed_record_field() :: {'typed_record_field', cerl_untyped_record_field(), cerl_type()}.
-type cerl_typedef() :: {name(), cerl_type(), [cerl_type_var()]}.

-type cerl_spec_form() :: {fa(), cerl_spec()}.
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

%% Intermediate types during form extraction and manipulation.
-type extracted_record_form() :: {lineno(), cerl_recdef()}.
-type extracted_type_form() :: {lineno(), cerl_typedef()}.

%% Types missing from the cerl module.
-type cerl_anno() :: {cerl:cerl(), cerl:cerl()}.


%% ===================================================================
%% External API
%% ===================================================================

%% Loads the AST of the given module and stores it as a kmodule().
-spec load(module(), tag_generator(), boolean()) -> {ok, kmodule()} | load_error().
load(M, TagGen, WithPmatch) ->
  case get_core(M, WithPmatch) of
    {ok, #c_module{}=AST} -> % just a sanity check that we get back a module
      {ok, kmodule(M, AST, TagGen)};
    {error, _} = Error -> Error
  end.

%% -------------------------------------------------------------------
%% kmodule API
%% -------------------------------------------------------------------

%% Constructs a kmodule.
%% It takes the AST of the module as input and performs all the computations
%% to produce a kmodule.
-spec kmodule(module(), cerl:cerl(), tag_generator()) -> kmodule().
kmodule(M, AST, TagGen) ->
  Kmodule = ets:new(M, [ordered_set, protected]),
  Exports = extract_exports(M, AST),
  Attrs = cerl:module_attrs(AST),
  RecordForms = extract_record_forms(Attrs),
  TypeForms = extract_type_forms(Attrs),
  SpecForms = extract_spec_forms(Attrs),
  Types = cuter_types:retrieve_types(TypeForms, RecordForms),
  Specs = cuter_types:retrieve_specs(SpecForms),
  Defs = cerl:module_defs(AST),
  Funs = [process_fundef(D, Exports, M, TagGen) || D <- Defs],
  ets:insert(Kmodule, {name, M}),
  ets:insert(Kmodule, {types, Types}),
  ets:insert(Kmodule, {specs, Specs}),
  ets:insert(Kmodule, {spec_forms, SpecForms}),
  ets:insert(Kmodule, {exported_types, extract_exported_types(M, Attrs)}),
  ets:insert(Kmodule, {type_forms, TypeForms}),
  ets:insert(Kmodule, {record_forms, RecordForms}),
  lists:foreach(fun({Mfa, Kfun}) -> ets:insert(Kmodule, {Mfa, Kfun}) end, Funs),
  Kmodule.

%% Returns a set of all the exported types of a kmodule.
%% Types are module-prefixed.
-spec kmodule_exported_types(kmodule()) -> sets:set({module(), atom(), arity()}).
kmodule_exported_types(Kmodule) ->
  [{exported_types, ExpTypes}] = ets:lookup(Kmodule, exported_types),
  ExpTypes.

%% Returns the name of a kmodule.
-spec kmodule_name(kmodule()) -> module().
kmodule_name(Kmodule) ->
  [{name, Name}] = ets:lookup(Kmodule, name),
  Name.

%% Returns the processed specs of a kmodule.
-spec kmodule_specs(kmodule()) -> cuter_types:stored_specs().
kmodule_specs(Kmodule) ->
  [{specs, Specs}] = ets:lookup(Kmodule, specs),
  Specs.

%% Returns the processed types of a kmodule.
-spec kmodule_types(kmodule()) -> cuter_types:stored_types().
kmodule_types(Kmodule) ->
  [{types, Types}] = ets:lookup(Kmodule, types),
  Types.

%% Returns the unprocessed types (and opaques) of a kmodule (as forms).
-spec kmodule_type_forms(kmodule()) -> [{lineno(), cerl:cerl()}].
kmodule_type_forms(Kmodule) ->
  [{type_forms, TypeForms}] = ets:lookup(Kmodule, type_forms),
  TypeForms.

%% Returns the unprocessed records of a kmodule (as forms).
-spec kmodule_record_forms(kmodule()) -> [{lineno(), {atom(), [cerl:cerl()]}}].
kmodule_record_forms(Kmodule) ->
  [{record_forms, RecordForms}] = ets:lookup(Kmodule, record_forms),
  RecordForms.

%% Returns the kfun for a given MFA of a kmodule.
-spec kmodule_kfun(kmodule(), mfa()) -> {ok, kfun()} | error.
kmodule_kfun(Kmodule, Mfa) ->
  case ets:lookup(Kmodule, Mfa) of
    [] -> error;
    [{Mfa, Kfun}] -> {ok, Kfun}
  end.

%% Returns the spec for a given MFA of a kmodule.
-spec kmodule_mfa_spec(kmodule(), mfa()) -> {ok, cuter_types:stored_spec_value()} | error.
kmodule_mfa_spec(Kmodule, {_M, F, A}) ->
  cuter_types:find_spec({F, A}, kmodule_specs(Kmodule)).

%% TODO(aggelos): Remove it when kmodule() becomes a map.
-spec destroy_kmodule(kmodule()) -> ok.
destroy_kmodule(Kmodule) ->
  ets:delete(Kmodule),
  ok.

%% Returns a dict with all the kfuns of a kmodule, that is keyed by MFAs.
-spec kmodule_mfas_with_kfuns(kmodule()) -> dict:dict(mfa(), kfun()).
kmodule_mfas_with_kfuns(Kmodule) ->
  Fn = fun({Key, Val}, Acc) ->
      case is_mfa(Key) of
        true -> dict:store(Key, Val, Acc);
        false -> Acc
      end
    end,
  ets:foldl(Fn, dict:new(), Kmodule).

is_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A), A >= 0 -> true;
is_mfa(_Mfa) -> false.

%% Returns the unprocessed specs of a kmodule (as forms).
-spec kmodule_spec_forms(kmodule()) -> [cerl_spec_form()].
kmodule_spec_forms(Kmodule) ->
  [{spec_forms, SpecsForms}] = ets:lookup(Kmodule, spec_forms),
  SpecsForms.

%% Returns a dict with all the unprocessed specs of a kmodule (as forms),
%% that is keyed by MFAs.
-spec kmodule_mfas_with_spec_forms(kmodule()) -> dict:dict(mfa(), any()).
kmodule_mfas_with_spec_forms(Kmodule) ->
  [{name, M}] = ets:lookup(Kmodule, name),
  SpecsForms = kmodule_spec_forms(Kmodule),
  Fn = fun({{F, A}, Spec}, Acc) ->
    dict:store({M, F, A}, Spec, Acc)
  end,
  lists:foldl(Fn, dict:new(), SpecsForms).

%% Updates the kfun of the given MFA of a kmodule.
-spec kmodule_update_kfun(kmodule(), mfa(), kfun()) -> true.
kmodule_update_kfun(Kmodule, MFa, Kfun) -> ets:insert(Kmodule, {MFa, Kfun}).

%% -------------------------------------------------------------------
%% kfun API
%% -------------------------------------------------------------------

%% Constructs a kfun.
-spec kfun(code(), boolean()) -> kfun().
kfun(Code, IsExported) ->
  #{code => Code, is_exported => IsExported}.

%% Returns if a kfun represents an exported MFA.
-spec kfun_is_exported(kfun()) -> boolean().
kfun_is_exported(#{is_exported := IsExported}) -> IsExported.

%% Returns the code of a kfun.
-spec kfun_code(kfun()) -> code().
kfun_code(#{code := Code}) -> Code.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec extract_exports(module(), ast()) -> [mfa()].
extract_exports(M, AST) ->
  Exports = cerl:module_exports(AST),
  [mfa_from_var(M, E) || E <- Exports].

extract_exported_types(Mod, Attrs) ->
  ExpTypes = lists:append([Ts || {#c_literal{val = export_type}, #c_literal{val = Ts}} <- Attrs]),
  sets:from_list([{Mod, Tname, Tarity} || {Tname, Tarity} <- ExpTypes]).

-spec process_fundef({cerl:c_var(), code()}, [mfa()], module(), tag_generator()) -> {mfa(), kfun()}.
process_fundef({FunVar, Def}, Exports, M, TagGen) ->
  Mfa = mfa_from_var(M, FunVar),
  IsExported = lists:member(Mfa, Exports),
  AnnDef = annotate(Def, TagGen),
  {Mfa, kfun(AnnDef, IsExported)}.

-spec mfa_from_var(module(), cerl:c_var()) -> mfa().
mfa_from_var(M, Var) ->
  {F, A} = cerl:var_name(Var),
  {M, F, A}.

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

%% Extracts the record definitions (as forms) from the annotations of a module.
%% The relevant annotations have the following structure in OTP 19.x and newer:
%%     {#c_atom{val=record}, #c_literal{val=[{Name, Fields}]}}
-spec extract_record_forms([cerl_anno()]) -> [extracted_record_form()].
extract_record_forms(Attrs) ->
  extract_record_forms(Attrs, []).

extract_record_forms([], Acc) -> lists:reverse(Acc);
extract_record_forms([{What, #c_literal{val = Val}=A}|Attrs], Acc) ->
  case cerl:atom_val(What) of
    Tp when Tp =:= type orelse Tp =:= opaque ->
      [V] = Val,
      Line = hd(cerl:get_ann(A)),
      case V of
        {{record, Name}, Fields, []} -> % for OTP 18.x and earlier
          extract_record_forms(Attrs, [{Line, {Name, Fields}}|Acc]);
        _ ->
          extract_record_forms(Attrs, Acc)
      end;
    record -> % for OTP 19.x and newer
      Line = hd(cerl:get_ann(A)),
      [V] = Val,
      extract_record_forms(Attrs, [{Line, V}|Acc]);
    _ ->
      extract_record_forms(Attrs, Acc)
  end.

%% Extracts the type definitions (as forms) from the annotations of a module.
%% The relevant annotations have the following structure:
%%   {#c_atom{val=type|opaque}, #c_literal{val=[{Name, Type, TypeVars}]}}
-spec extract_type_forms([cerl_anno()]) -> [extracted_type_form()].
extract_type_forms(Attrs) ->
  extract_type_forms(Attrs, []).

extract_type_forms([], Acc) ->
  lists:reverse(Acc);
extract_type_forms([{What, #c_literal{val = Val}=A}|Attrs], Acc) ->
  case cerl:atom_val(What) of
    Tp when Tp =:= type orelse Tp =:= opaque ->
      [V] = Val,
      Line = hd(cerl:get_ann(A)),
      case V of
        {{record, _Name}, _Fields, []} -> % for OTP 18.x and earlier
	        extract_type_forms(Attrs, Acc);
        _ ->
          extract_type_forms(Attrs, [{Line, V}|Acc])
      end;
    _ ->
      extract_type_forms(Attrs, Acc)
  end.

extract_spec_forms(Attrs) ->
  extract_spec_forms(Attrs, []).

extract_spec_forms([], Acc) ->
  lists:reverse(Acc);
extract_spec_forms([{What, #c_literal{val = Val}}|Attrs], Acc) ->
  case cerl:atom_val(What) of
    spec ->
      [V] = Val,
      extract_spec_forms(Attrs, [V|Acc]);
    _ ->
      extract_spec_forms(Attrs, Acc)
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
      %% TODO Annotate applications for lambda terms.
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
