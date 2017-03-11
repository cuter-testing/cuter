%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types).

-export([parse_spec/4, retrieve_types/1, retrieve_specs/1, find_spec/2, get_kind/1,
         find_remote_deps_of_type/2, find_remote_deps_of_spec/2]).

-export([params_of_t_function_det/1, ret_of_t_function/1, atom_of_t_atom_lit/1, integer_of_t_integer_lit/1,
         elements_type_of_t_list/1, elements_type_of_t_nonempty_list/1, elements_types_of_t_tuple/1,
         elements_types_of_t_union/1, bounds_of_t_range/1, segment_size_of_bitstring/1, is_generic_function/1,
         name_of_t_userdef/1]).

-export([t_atom/0, t_atom_lit/1, t_any/0, t_binary/0, t_bitstring/0, t_bitstring/2, t_char/0, t_float/0,
         t_function/0, t_function/2, t_function/3, t_integer/0, t_integer_lit/1, t_list/0, t_list/1,
         t_nonempty_list/1, t_nil/0, t_number/0, t_remote/3, t_string/0, t_tuple/0, t_tuple/1,
         t_union/1, t_range/2, t_pos_inf/0, t_neg_inf/0, t_userdef/1]).

-export([erl_type_deps_map/2, get_type_name_from_type_dep/1, get_type_from_type_dep/1, unique_type_name/3]).

-export_type([erl_type/0, erl_spec_clause/0, erl_spec/0, stored_specs/0, stored_types/0, stored_spec_value/0, t_range_limit/0]).
-export_type([erl_type_dep/0, erl_type_deps/0]).

-include("include/cuter_macros.hrl").
-include("include/cuter_types.hrl").

%% ============================================================================
%% Type Declations
%% ============================================================================

%% Define tags
-define(type_variable, vart).
-define(type_var, tvar).
-define(max_char, 16#10ffff).

%% Pre-processed types.

-type type_name() :: atom().
-type type_arity() :: byte().
-type type_var() :: {?type_var, atom()}.
-type remote_type() :: {module(), type_name(), type_arity()}.
-type dep() :: remote_type().
-type deps() :: ordsets:ordset(remote_type()).
-record(t, {
  kind,
  rep = undefined,
  deps = ordsets:new() :: deps()
}).

%% ----------------------------------------------------------------------------
%% Define the type of intermediate representation of types for CutEr.
%% ----------------------------------------------------------------------------

%% erl_type() is basically an overapproximation of the types that can be
%% encoded as a JSON string by the cuter_json module.
%% Basically, we exclude the types that need to be resolved, like user-defined
%% types. This is an approximation as there can be non-resolved types nested
%% within an erl_type().
-type erl_type() :: t_any()               % any()
                  | t_nil()               % []
                  | t_atom()              % atom()
                  | t_atom_lit()          % Erlang_Atom
                  | t_integer()           % integer(), +infinity, -inifinity
                  | t_integer_lit()       % Erlang_Integer
                  | t_float()             % float()
                  | t_tuple()             % tuple(), {TList}
                  | t_list()              % list(Type)
                  | t_nonempty_list()     % nonempty_list(Type)
                  | t_union()             % Type1 | ... | TypeN
                  | t_range()             % Erlang_Integer..Erlang_Integer
                  | t_bitstring()         % <<_:M>>
                  | t_function()          % function() | Fun | BoundedFun
                  | t_userdef()           % User defined type.
                  .

%% raw_type() represents every possible type or record supported.
-type raw_type() :: erl_type()
                  | t_local()             % Local Type Usage
                  | t_remote()            % Remote Type Usage
                  | t_record()            % Record Usage
                  | t_type_var()          % Type Variable
                  .

%% ----------------------------------------------------------------------------
%% The actual intermediate representation of types for CutEr (per type).
%% ----------------------------------------------------------------------------

%% Simple base types.
-type t_any()         :: #t{kind :: ?any_tag}.
-type t_nil()         :: #t{kind :: ?nil_tag}.
-type t_atom()        :: #t{kind :: ?atom_tag}.
-type t_atom_lit()    :: #t{kind :: ?atom_lit_tag, rep :: atom()}.
-type t_integer()     :: #t{kind :: ?integer_tag}.
-type t_integer_lit() :: #t{kind :: ?integer_lit_tag, rep :: integer()}.
-type t_float()       :: #t{kind :: ?float_tag}.

%% Simple compound types.
-type t_tuple()         :: #t{kind :: ?tuple_tag, rep :: [raw_type()]}.
-type t_list()          :: #t{kind :: ?list_tag, rep :: raw_type()}.
-type t_nonempty_list() :: #t{kind :: ?nonempty_list_tag, rep :: raw_type()}.
-type t_union()         :: #t{kind :: ?union_tag, rep :: [raw_type()]}.

%% Range of integers.
-type t_range()           :: #t{kind :: ?range_tag, rep :: {t_range_limit(), t_range_limit()}}.
-type t_range_limit()     :: t_integer_lit() | t_integer_inf().
-type t_integer_inf()     :: t_integer_pos_inf() | t_integer_neg_inf().
-type t_integer_pos_inf() :: #t{kind :: ?pos_inf}.
-type t_integer_neg_inf() :: #t{kind :: ?neg_inf}.

%% Bitstrings.
-type seg_sizes()   :: {non_neg_integer(), non_neg_integer()}.
-type t_bitstring() :: #t{kind :: ?bitstring_tag, rep :: seg_sizes()}.

%% Funs.
-type t_function()     :: t_function_gen() | t_function_det().
-type t_function_gen() :: #t{kind :: ?function_tag, rep :: {raw_type(), [t_constraint()]}, deps :: deps()}.
-type t_function_det() :: #t{kind :: ?function_tag, rep :: {[raw_type()], raw_type(), [t_constraint()]}, deps :: deps()}.
-type t_constraint()   :: {t_type_var(), raw_type()}.

%% User-defined types (module-local and remote).
-type t_local()  :: #t{kind :: ?local_tag, rep :: {type_name(), [raw_type()]}}.
-type t_remote() :: #t{kind :: ?remote_tag, rep :: {module(), type_name(), [raw_type()]}}.
-type t_userdef() :: #t{kind :: ?userdef_tag, rep :: string()}.

%% Records.
-type t_record()          :: #t{kind :: ?record_tag, rep :: {record_name(), [record_field_type()]}}.
-type record_name()       :: atom().
-type record_field_type() :: {record_field_name(), raw_type()}.
-type record_field_name() :: atom().

%% Type variable.
-type t_type_var() :: #t{kind :: ?type_variable, rep :: type_var()}.

%% ----------------------------------------------------------------------------
%% How intermediate type & spec representations are stored.
%% ----------------------------------------------------------------------------

%% The stored_types() is the signature of the cache that holds the intermediate
%% representation of types in CutEr.
%% It holds all the types and records declared in the supplied attributes.
-type stored_types() :: dict:dict(stored_type_key(), stored_type_value()).
-type stored_type_key() :: {record, record_name()} | {type, type_name(), type_arity()}.
-type stored_type_value() :: [record_field_type()] | {any(), [type_var()]}. % raw_type()

%% The stored_specs() is the signature of the cache that holds the intermediate
%% representation of specs in CutEr.
-type stored_spec_key() :: {type_name(), type_arity()}.
-type stored_spec_value() :: [t_function_det()].
-type stored_specs() :: dict:dict(stored_spec_key(), stored_spec_value()).

%% ----------------------------------------------------------------------------
%% Useful types in spec parsing.
%% ----------------------------------------------------------------------------

%% A list of key-value pairs. Each pair represented the types' cache of the
%% given module.
-type many_stored_types()       :: [{atom(), stored_types()}].
%% A cache of modules and their types' caches.
%% It's a transformed version of many_stored_types().
-type many_stored_types_cache() :: dict:dict(atom(), stored_types()).

%% The data types that holds the configuration info of spec parsing.
-record(pconf, {
  mfa            :: mfa(),
  typesCache     :: many_stored_types_cache(),
  typeDepsCache  :: ets:tid()
}).
-type parse_conf() :: #pconf{}.

-type unique_type_name() :: string().
-type type_var_env() :: dict:dict(type_var(), raw_type()).
-type erl_type_dep() :: {unique_type_name(), erl_type()}.
-type erl_type_deps() :: [erl_type_dep()].
-type erl_spec_clause() :: t_function_det().
-type erl_spec() :: {[erl_spec_clause()], erl_type_deps()}.

%% ============================================================================
%% Pre-process the type & record declarations and generate their intermediate
%% representation from abstract forms.
%% ============================================================================

%% Pre-processes the type & record declarations.
-spec retrieve_types([cuter_cerl:type_info()]) -> stored_types().
retrieve_types(TypeAttrs) ->
  lists:foldl(fun process_type_attr/2, dict:new(), TypeAttrs).

-spec process_type_attr(cuter_cerl:type_info(), stored_types()) -> stored_types().
%% Processes the declaration of a record.
process_type_attr({record, {Name, Fields}}, Processed) ->
  % Process each field of the record.
  Fs = [t_field_from_form(Field) || Field <- Fields],
  % Construct the representation of the record.
  Record = t_record(Name, Fs),
  % Store the record in the proccessed dict.
  dict:store({record, Name}, Record, Processed);
%% Processes the declaration of a type.
process_type_attr({type, {Name, Repr, Vars}}, Processed) ->
  % Process the type's representation.
  Type = safe_t_from_form(Repr),
  % Parse the type parameters.
  Vs = [{?type_var, Var} || {var, _, Var} <- Vars],
  % Store the type in the processed dict.
  dict:store({type, Name, length(Vs)}, {Type, Vs}, Processed).

%% Processes the declaration of a record's field.
-spec t_field_from_form(cuter_cerl:cerl_record_field()) -> record_field_type().
%% A simple field.
t_field_from_form({record_field, _, {atom, _, Name}}) ->
  {Name, t_any()};
%% A simple field with a default value.
t_field_from_form({record_field, _, {atom, _, Name}, _Default}) ->
  {Name, t_any()};
%% A typed field.
t_field_from_form({typed_record_field, {record_field, _, {atom, _, Name}}, Type}) ->
  {Name, safe_t_from_form(Type)};
%% A typed field with a default value.
t_field_from_form({typed_record_field, {record_field, _, {atom, _, Name}, _Default}, Type}) ->
  {Name, safe_t_from_form(Type)}.

%% Acts a fallback in case an unsupported type is encountered.
%% In this case, the type is generalized to any().
safe_t_from_form(Form) ->
  try t_from_form(Form)
  catch throw:{unsupported, Info} ->
    cuter_pp:form_has_unsupported_type(Info),
    t_any()
  end.

%% Parses the declaration of a type.
-spec t_from_form(cuter_cerl:cerl_type()) -> raw_type().
%% An Erlang atom literal
t_from_form({atom, _, Atom}) when is_atom(Atom) ->
  t_atom_lit(Atom);
%% An Erlang integer literal
t_from_form({integer, _, Integer}) when is_integer(Integer) ->
  t_integer_lit(Integer);
%% integer()
t_from_form({type, _, integer, []}) ->
  t_integer();
%% nil
t_from_form({type, _, nil, []}) ->
  t_nil();
%% any()
t_from_form({type, _, any, []}) ->
  t_any();
%% term()
t_from_form({type, _, term, []}) ->
  t_any();
%% atom()
t_from_form({type, _, atom, []}) ->
  t_atom();
%% module()
t_from_form({type, _, module, []}) ->
  t_module();
%% float()
t_from_form({type, _, float, []}) ->
  t_float();
%% tuple()
t_from_form({type, _, tuple, any}) ->
  t_tuple();
%% { TList } when TList :: T1, T2, ... , Tn
t_from_form({type, _, tuple, Types}) ->
  %% FIXME Distinguish between tuple() and {}.
  Ts = [t_from_form(T) || T <- Types],
  t_tuple(Ts);
%% list()
t_from_form({type, _, list, []}) ->
  t_list();
%% list(Type)
t_from_form({type, _, list, [Type]}) ->
  T = t_from_form(Type),
  t_list(T);
%% T1 | ... | Tn
t_from_form({type, _, union, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_union(Ts);
%% boolean()
t_from_form({type, _, boolean, []}) ->
  t_boolean();
%% number()
t_from_form({type, _, number, []}) ->
  t_number();
%% range, i.e. Int1..Int2
t_from_form({type, _, range, [{integer, _, I1}, {integer, _, I2}]}) ->
  L = t_integer_lit(I1),
  R = t_integer_lit(I2),
  t_range(L, R);
t_from_form({type, _, range, [{op, _, '-', {integer, _, I1}}, {integer, _, I2}]}) ->
  L = t_integer_lit(-I1),
  R = t_integer_lit(I2),
  t_range(L, R);
t_from_form({type, _, range, [{integer, _, I1}, {op, _, '-', {integer, _, I2}}]}) ->
  L = t_integer_lit(I1),
  R = t_integer_lit(-I2),
  t_range(L, R);
t_from_form({type, _, range, [{op, _, '-', {integer, _, I1}}, {op, _, '-', {integer, _, I2}}]}) ->
  L = t_integer_lit(-I1),
  R = t_integer_lit(-I2),
  t_range(L, R);
%% non_neg_integer()
t_from_form({type, _, non_neg_integer, []}) ->
  t_non_neg_integer();
%% pos_integer()
t_from_form({type, _, pos_integer, []}) ->
  t_pos_integer();
%% neg_integer()
t_from_form({type, _, neg_integer, []}) ->
  t_neg_integer();
%% char()
t_from_form({type, _, char, []}) ->
  t_char();
%% arity()
t_from_form({type, _, arity, []}) ->
  t_byte();
%% byte()
t_from_form({type, _, byte, []}) ->
  t_byte();
%% mfa()
t_from_form({type, _, mfa, []}) ->
  t_mfa();
%% string()
t_from_form({type, _, string, []}) ->
  t_string();
%% nonempty_list()
t_from_form({type, _, nonempty_list, []}) ->
  t_nonempty_list();
%% nonempty_list(Type)
t_from_form({type, _, nonempty_list, [Type]}) ->
  T = t_from_form(Type),
  t_nonempty_list(T);
%% <<_:M, _:_*N>>
t_from_form({type, _, binary, [{integer, _, M}, {integer, _, N}]}) ->
  t_bitstring(M, N);
%% binary()
t_from_form({type, _, binary, []}) ->
  t_binary();
%% bitstring()
t_from_form({type, _, bitstring, []}) ->
  t_bitstring();
%% function()
t_from_form({type, _, function, []}) ->
  t_function();
%% fun((TList) -> Type) | fun((...) -> Type)
t_from_form({type, _, 'fun', [_Product, _RetType]}=Fun) ->
  t_function_from_form(Fun);
%% fun((TList) -> Type) | fun((...) -> Type) (bounded_fun)
t_from_form({type, _, 'bounded_fun', [_Fun, _Cs]}=BoundedFun) ->
  t_bounded_function_from_form(BoundedFun);
%% ann_type
t_from_form({ann_type, _, [_Var, Type]}) ->
  t_from_form(Type);
%% paren_type
t_from_form({paren_type, _, [Type]}) ->
  t_from_form(Type);
%% remote_type
t_from_form({remote_type, _, [{atom, _, M}, {atom, _, Name}, Types]}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_remote(M, Name, Ts);
%% Record
t_from_form({type, _, record, [{atom, _, Name} | FieldTypes]}) ->
  Fields = [t_bound_field_from_form(F) || F <- FieldTypes],
  t_record(Name, Fields);
%% Map
t_from_form({type, _, map, _}=X) ->
  throw({unsupported, X});
%% local type
t_from_form({Tag, _, Name, Types}) when Tag =:= type; Tag =:= user_type ->
  Ts = [t_from_form(T) || T <- Types],
  t_local(Name, Ts);
%% Type Variable
t_from_form({var, _, Var}) ->
  t_var(Var);
%% Unsupported forms
t_from_form(Type) ->
  throw({unsupported, Type}).

%% Parses a record's field that is referred in a type.
-spec t_bound_field_from_form(cuter_cerl:cerl_type_record_field()) -> record_field_type().
t_bound_field_from_form({type, _, field_type, [{atom, _, Name}, Type]}) ->
  {Name, t_from_form(Type)}.

%% Parses the declaration of a simple fun.
-spec t_function_from_form(cuter_cerl:cerl_func()) -> t_function_det().
t_function_from_form({type, _, 'fun', [{type, _, 'product', Types}, RetType]}) ->
  Ret = safe_t_from_form(RetType),
  Ts = [safe_t_from_form(T) || T <- Types],
  t_function(Ts, Ret);
t_function_from_form({type, _, 'fun', [{type, _, any}, RetType]}) ->
  Ret = safe_t_from_form(RetType),
  t_function(Ret, []).

%% Parses the declaration of a bounded fun.
-spec t_bounded_function_from_form(cuter_cerl:cerl_bounded_func()) -> t_function_det().
t_bounded_function_from_form({type, _, 'bounded_fun', [Fun, Constraints]}) ->
  Cs = [t_constraint_from_form(C) || C <- Constraints],
  case Fun of
    {type, _, 'fun', [{type, _, any}, RetType]} ->
      Ret = safe_t_from_form(RetType),
      t_function(Ret, Cs);
    {type, _, 'fun', [{type, _, 'product', Types}, RetType]} ->
      Ret = safe_t_from_form(RetType),
      Ts = [safe_t_from_form(T) || T <- Types],
      t_function(Ts, Ret, Cs)
  end.

%% Parses the constraints of a bounded fun.
-spec t_constraint_from_form(cuter_cerl:cerl_constraint()) -> t_constraint().
t_constraint_from_form({type, _, constraint, [{atom, _, is_subtype}, [{var, _, Var}, Type]]}) ->
  {t_var(Var), safe_t_from_form(Type)}.

%% ----------------------------------------------------------------------------
%% API for types constructors.
%% ----------------------------------------------------------------------------

%% Basic constructors.

-spec t_any() -> t_any().
t_any() ->
  #t{kind = ?any_tag}.

-spec t_atom_lit(atom()) -> t_atom_lit().
t_atom_lit(Atom) ->
  #t{kind = ?atom_lit_tag, rep = Atom}.

-spec t_atom() -> t_atom().
t_atom() ->
  #t{kind = ?atom_tag}.

-spec t_integer_lit(integer()) -> t_integer_lit().
t_integer_lit(Integer) ->
  #t{kind = ?integer_lit_tag, rep = Integer}.

-spec t_integer() -> t_integer().
t_integer() ->
  #t{kind = ?integer_tag}.

-spec t_range(t_range_limit(), t_range_limit()) -> t_range().
t_range(Int1, Int2) ->
  #t{kind = ?range_tag, rep = {Int1, Int2}}.

-spec t_pos_inf() -> t_integer_pos_inf().
t_pos_inf() ->
  #t{kind = ?pos_inf}.

-spec t_neg_inf() -> t_integer_neg_inf().
t_neg_inf() ->
  #t{kind = ?neg_inf}.

-spec t_nil() -> t_nil().
t_nil() ->
  #t{kind = ?nil_tag}.

-spec t_float() -> t_float().
t_float() ->
  #t{kind = ?float_tag}.

-spec t_list() -> t_list().
t_list() ->
  #t{kind = ?list_tag, rep = t_any()}.

-spec t_list(raw_type()) -> t_list().
t_list(Type) ->
  #t{kind = ?list_tag, rep = Type, deps = get_deps(Type)}.

-spec t_nonempty_list() -> t_nonempty_list().
t_nonempty_list() ->
  #t{kind = ?nonempty_list_tag, rep = t_any()}.

-spec t_nonempty_list(raw_type()) -> t_nonempty_list().
t_nonempty_list(Type) ->
  #t{kind = ?nonempty_list_tag, rep = Type, deps = get_deps(Type)}.

-spec t_tuple() -> t_tuple().
t_tuple() ->
  #t{kind = ?tuple_tag, rep = []}.

-spec t_tuple([raw_type()]) -> t_tuple().
t_tuple(Types) ->
  #t{kind = ?tuple_tag, rep = Types, deps = unify_deps(Types)}.

-spec t_union([raw_type()]) -> t_union().
t_union(Types) ->
  #t{kind = ?union_tag, rep = Types, deps = unify_deps(Types)}.

-spec t_local(type_name(), [raw_type()]) -> t_local().
t_local(Name, Types) ->
  Rep = {Name, Types},
  #t{kind = ?local_tag, rep = Rep, deps = unify_deps(Types)}.

-spec t_remote(module(), type_name(), [raw_type()]) -> t_remote().
t_remote(Mod, Name, Types) ->
  Rep = {Mod, Name, Types},
  Dep = {Mod, Name, length(Types)},
  #t{kind = ?remote_tag, rep = Rep, deps = add_dep(Dep, unify_deps(Types))}.

-spec t_var(atom()) -> t_type_var().
t_var(Var) ->
  #t{kind = ?type_variable, rep = {?type_var, Var}}.

-spec t_record(record_name(), [record_field_type()]) -> t_record().
t_record(Name, Fields) ->
  Rep = {Name, Fields},
  Ts = [T || {_, T} <- Fields],
  #t{kind = ?record_tag, rep = Rep, deps = unify_deps(Ts)}.

-spec t_bitstring() -> t_bitstring().
t_bitstring() ->
  t_bitstring(0, 1).

-spec t_bitstring(non_neg_integer(), non_neg_integer()) -> t_bitstring().
t_bitstring(M, N) ->
  #t{kind = ?bitstring_tag, rep = {M, N}}.

-spec t_binary() -> t_bitstring().
t_binary() ->
  t_bitstring(0, 8).

-spec t_function() -> t_function_gen().
t_function() ->
  Rep = {t_any(), []},
  #t{kind = ?function_tag, rep = Rep}.

-spec t_function([raw_type()], raw_type()) -> t_function_det()
              ; (raw_type(), [t_constraint()]) -> t_function_gen().
t_function(Types, Ret) when is_list(Types) ->
  Rep = {Types, Ret, []},
  #t{kind = ?function_tag, rep = Rep, deps = unify_deps([Ret|Types])};
t_function(Ret, Constraints) ->
  Rep = {Ret, Constraints},
  Ts = [T || {_V, T} <- Constraints],
  #t{kind = ?function_tag, rep = Rep, deps = unify_deps([Ret|Ts])}.

-spec t_function([raw_type()], raw_type(), [t_constraint()]) -> t_function_det().
t_function(Types, Ret, Constraints) ->
  Rep = {Types, Ret, Constraints},
  Ts = [T || {_V, T} <- Constraints],
  #t{kind = ?function_tag, rep = Rep, deps = unify_deps([Ret|Types] ++ Ts)}.

-spec t_userdef(string()) -> t_userdef().
t_userdef(Name) ->
  #t{kind = ?userdef_tag, rep = Name}.

%% Constructors that are essentially aliases.

-spec t_boolean() -> t_union().
t_boolean() ->
  Alts = [t_atom_lit(true), t_atom_lit(false)],
  t_union(Alts).

-spec t_byte() -> t_range().
t_byte() ->
  t_range(t_integer_lit(0), t_integer_lit(255)).

-spec t_mfa() -> t_tuple().
t_mfa() ->
  Ts = [t_module(), t_atom(), t_byte()],
  t_tuple(Ts).

-spec t_module() -> t_atom().
t_module() ->
  t_atom().

-spec t_neg_integer() -> t_range().
t_neg_integer() ->
  L = t_neg_inf(),
  R = t_integer_lit(-1),
  t_range(L, R).

-spec t_non_neg_integer() -> t_range().
t_non_neg_integer() ->
  L = t_integer_lit(0),
  R = t_pos_inf(),
  t_range(L, R).

-spec t_pos_integer() -> t_range().
t_pos_integer() ->
  L = t_integer_lit(1),
  R = t_pos_inf(),
  t_range(L, R).

-spec t_number() -> t_union().
t_number() ->
  Alts = [t_integer(), t_float()],
  t_union(Alts).

-spec t_char() -> t_range().
t_char() ->
  L = t_integer_lit(0),
  R = t_integer_lit(?max_char),
  t_range(L, R).

-spec t_string() -> t_list().
t_string() ->
  t_list(t_char()).

%% ----------------------------------------------------------------------------
%% API for type accessors.
%% ----------------------------------------------------------------------------

-spec is_generic_function(t_function()) -> boolean().
is_generic_function(#t{kind = ?function_tag, rep = {_, _, _}}) ->
  false;
is_generic_function(#t{kind = ?function_tag, rep = {_, _}}) ->
  true.

-spec fields_of_t_record(t_record()) -> [record_field_type()].
fields_of_t_record(Record) ->
  Rep = Record#t.rep,
  element(2, Rep).

-spec params_of_t_function_det(t_function_det()) -> [raw_type()].
params_of_t_function_det(#t{kind = ?function_tag, rep = {Params, _Ret, _Constraints}}) ->
  Params.

-spec ret_of_t_function(t_function()) -> raw_type().
ret_of_t_function(#t{kind = ?function_tag, rep = {_Params, Ret, _Constraints}}) ->
  Ret;
ret_of_t_function(#t{kind = ?function_tag, rep = {Ret, _Constraints}}) ->
  Ret.

-spec atom_of_t_atom_lit(t_atom_lit()) -> atom().
atom_of_t_atom_lit(#t{kind = ?atom_lit_tag, rep = Atom}) ->
  Atom.

-spec integer_of_t_integer_lit(t_integer_lit()) -> integer().
integer_of_t_integer_lit(#t{kind = ?integer_lit_tag, rep = Integer}) ->
  Integer.

-spec elements_type_of_t_list(t_list()) -> raw_type().
elements_type_of_t_list(#t{kind = ?list_tag, rep = Type}) ->
  Type.

-spec elements_type_of_t_nonempty_list(t_nonempty_list()) -> raw_type().
elements_type_of_t_nonempty_list(#t{kind = ?nonempty_list_tag, rep = Type}) ->
  Type.

-spec elements_types_of_t_tuple(t_tuple()) -> [raw_type()].
elements_types_of_t_tuple(#t{kind = ?tuple_tag, rep = Types}) ->
  Types.

-spec elements_types_of_t_union(t_union()) -> [raw_type()].
elements_types_of_t_union(#t{kind = ?union_tag, rep = Types}) ->
  Types.

-spec bounds_of_t_range(t_range()) -> {t_range_limit(), t_range_limit()}.
bounds_of_t_range(#t{kind = ?range_tag, rep = Limits}) ->
  Limits.

-spec segment_size_of_bitstring(t_bitstring()) -> seg_sizes().
segment_size_of_bitstring(#t{kind = ?bitstring_tag, rep = Sz}) ->
  Sz.

-spec is_tvar_wild_card(t_type_var()) -> boolean().
is_tvar_wild_card(#t{kind = ?type_variable, rep = {?type_var, Var}}) ->
  Var =:= '_'.

-spec name_of_t_userdef(t_userdef()) -> string().
name_of_t_userdef(#t{kind = ?userdef_tag, rep = Name}) ->
  Name.

%% ----------------------------------------------------------------------------
%% Generic API for raw_type().
%% ----------------------------------------------------------------------------

-spec get_kind(raw_type()) -> atom().
get_kind(Type) ->
  Type#t.kind.

-spec get_deps(raw_type()) -> deps().
get_deps(Type) ->
  Type#t.deps.

-spec add_dep(dep(), deps()) -> deps().
add_dep(Dep, Deps) ->
  ordsets:add_element(Dep, Deps).

-spec unify_deps([raw_type()]) -> deps().
unify_deps(Types) ->
  ordsets:union([T#t.deps || T <- Types]).

%% ============================================================================
%% Pre-process the spec declarations.
%% A spec is essentially a list of function types, thus there is no need to
%% define an intermediate representation solely for specs.
%% ============================================================================

-spec retrieve_specs([cuter_cerl:spec_info()]) -> stored_specs().
retrieve_specs(SpecAttrs) ->
  lists:foldl(fun process_spec_attr/2, dict:new(), SpecAttrs).

-spec process_spec_attr(cuter_cerl:spec_info(), stored_specs()) -> stored_specs().
process_spec_attr({FA, Specs}, Processed) ->
  Xs = [t_spec_from_form(Spec) || Spec <- Specs],
  dict:store(FA, Xs, Processed).

-spec t_spec_from_form(cuter_cerl:cerl_spec_func()) -> t_function_det().
t_spec_from_form({type, _, 'fun', _}=Fun) ->
  t_function_from_form(Fun);
t_spec_from_form({type, _, 'bounded_fun', _}=Fun) ->
  t_bounded_function_from_form(Fun).

%% ----------------------------------------------------------------------------
%% Lookup a function's spec from the module's caches of pre-processed specs.
%% ----------------------------------------------------------------------------

-spec find_spec(stored_spec_key(), stored_specs()) -> {'ok', stored_spec_value()} | 'error'.
find_spec(FA, Specs) ->
  dict:find(FA, Specs).

%% ============================================================================
%% Traverse a pre-processed spec and substitute the user-defined types.
%% FIXME: Does not work for recursive types.
%% ============================================================================

%% ----------------------------------------------------------------------------
%% API for the configuration of a spec's parsing & simplification.
%% ----------------------------------------------------------------------------

%% Creates a cache that contains the stored_types() of each module.
%% It takes a key-value list of modules and their stored types and returns
%% the cache.
-spec stored_types_of_modules(many_stored_types()) -> many_stored_types_cache().
stored_types_of_modules(Ms) ->
  stored_types_of_modules(Ms, dict:new()).

stored_types_of_modules([], Acc) ->
  Acc;
stored_types_of_modules([{Module, StoredTypes}|Rest], Acc) ->
  stored_types_of_modules(Rest, dict:store(Module, StoredTypes, Acc)).

%% Creates the configuration.
-spec mk_conf(mfa(), many_stored_types()) -> parse_conf().
mk_conf(Mfa, ManyStoredTypes) ->
  Cache = stored_types_of_modules(ManyStoredTypes),
  DepsCache = ets:new(?MODULE, [ordered_set, protected]),
  true = ets:insert(DepsCache, {parsed, []}),
  true = ets:insert(DepsCache, {pending, []}),
  #pconf{mfa = Mfa, typesCache = Cache, typeDepsCache = DepsCache}.

%% Accesses the base mfa.
getMfa(Conf) ->
  Conf#pconf.mfa.

%% Accesses the type deps.
getTypeDeps(Conf) ->
  Cache = Conf#pconf.typeDepsCache,
  [{parsed, Parsed}] = ets:lookup(Cache, parsed),
  Parsed.

%% Stores a parsed type dep.
store_parsed_type(Conf, Name, Type) ->
  Cache = Conf#pconf.typeDepsCache,
  [{parsed, Parsed}] = ets:lookup(Cache, parsed),
  true = ets:insert(Cache, {parsed, [{Name, Type}|Parsed]}).

%% Checks if there are pending types to parse.
hasPendingTypes(Conf) ->
  Cache = Conf#pconf.typeDepsCache,
  [{pending, []}] =/= ets:lookup(Cache, pending).

%% Adds a pending type dep.
add_pending_type(Conf, Name, Fn) ->
  Cache = Conf#pconf.typeDepsCache,
  case ets:lookup(Cache, Name) of
    [{Name, true}] ->
      true;
    [] ->
      [{pending, Pending}] = ets:lookup(Cache, pending),
      true = ets:insert(Cache, {pending, [{Name, Fn}|Pending]}),
      true = ets:insert(Cache, {Name, true})
  end.

%% Gets the next pending type to parse.
next_pending_type(Conf) ->
  Cache = Conf#pconf.typeDepsCache,
  case ets:lookup(Cache, pending) of
    [{pending, []}] -> error;
    [{pending, [T|Ts]}] ->
      % T :: {Name :: string(), Fn :: function()}
      true = ets:insert(Cache, {pending, Ts}),
      T
  end.

%% Accesses the stored_types() of a particular module.
getTypesCacheOfModule(Module, Conf) ->
  case dict:find(Module, Conf#pconf.typesCache) of
    error -> dict:new(); % TODO Use a constructor for an empty stored_types().
    {ok, StoredTypes} -> StoredTypes
  end.

-spec cleanup_conf(parse_conf()) -> true.
cleanup_conf(#pconf{typeDepsCache = Cache}) ->
  ets:delete(Cache).

-spec unique_type_name(atom(), atom(), [any()]) -> string().
unique_type_name(Mod, TypeName, Args) ->
  Ps = [atom_to_list(Mod), atom_to_list(TypeName)],
  Ps1 = maybe_hash_args(Ps, Args),
  string:join(Ps1, "@").

maybe_hash_args(Ps, []) ->
  Ps;
maybe_hash_args(Ps, Args) ->
  Arity = length(Args),
  Ps ++ [integer_to_list(Arity), integer_to_list(erlang:phash2(Args))].

%% ----------------------------------------------------------------------------
%% Normalize parsed specs & type dependencies
%% ----------------------------------------------------------------------------

normalize_type_deps(Deps) ->
  Seen = ets:new(?MODULE, [ordered_set, protected]),
  NormalizedDeps = normalize_type_deps(Deps, Seen, []),
  ets:delete(Seen),
  NormalizedDeps.

normalize_type_deps([], _Seen, Acc) ->
  lists:reverse(Acc);
normalize_type_deps([{TypeName, Type}|Types], Seen, Acc) ->
  {NormalizedType, MoreDeps} = normalize_single_type(Type, Seen, true),
  Acc1 = lists:reverse(MoreDeps, Acc),
  normalize_type_deps(Types, Seen, [{TypeName, NormalizedType}|Acc1]).

normalize_single_type(#t{kind = ?function_tag, rep = {Params, Ret, _}}=Type, Seen, IsTopLevel) ->
  case ets:lookup(Seen, Type) of
    [{Type, Handle}] ->
      {Handle, []};
    [] ->
      Xs = [normalize_single_type(T, Seen, false) || T <- [Ret|Params]],
      {[T|Ts], Deps} = lists:unzip(Xs),
      NormalizedType = Type#t{rep = {Ts, T, []}},
      AllDeps = lists:flatten([lists:reverse(D) || D <- Deps]),
      insert_userdef_and_update_seen(Seen, IsTopLevel, NormalizedType, AllDeps)
  end;
normalize_single_type(#t{kind = ?function_tag, rep = {Ret, _}}=Type, Seen, IsTopLevel) ->
  case ets:lookup(Seen, Type) of
    [{Type, Handle}] ->
      {Handle, []};
    [] ->
      {T, Deps} = normalize_single_type(Ret, Seen, false),
      NormalizedType = Type#t{rep = {T, []}},
      insert_userdef_and_update_seen(Seen, IsTopLevel, NormalizedType, Deps)
  end;
%% list / nonempty_list
normalize_single_type(#t{kind = Tag, rep = InnerType}=Type, Seen, IsTopLevel) when Tag =:= ?list_tag; Tag =:= ?nonempty_list_tag ->
  case ets:lookup(Seen, Type) of
    [{Type, Handle}] ->
      {Handle, []};
    [] ->
      {T, Deps} = normalize_single_type(InnerType, Seen, false),
      NormalizedType = Type#t{rep = T},
      insert_userdef_and_update_seen(Seen, IsTopLevel, NormalizedType, Deps)
  end;
%% union or tuple
normalize_single_type(#t{kind = Tag, rep = InnerTypes}=Type, Seen, IsTopLevel) when Tag =:= ?union_tag; Tag =:= ?tuple_tag ->
  case ets:lookup(Seen, Type) of
    [{Type, Handle}] ->
      {Handle, []};
    [] ->
      Xs = [normalize_single_type(T, Seen, false) || T <- InnerTypes],
      {Ts, Deps} = lists:unzip(Xs),
      NormalizedType = Type#t{rep = Ts},
      AllDeps = lists:flatten([lists:reverse(D) || D <- Deps]),
      insert_userdef_and_update_seen(Seen, IsTopLevel, NormalizedType, AllDeps)
  end;
%% all others
normalize_single_type(Raw, _Seen, _IsTopLevel) ->
  {Raw, []}.

generate_new_type() ->
  "tp@" ++ erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>".

insert_userdef_and_update_seen(Seen, IsTopLevel, NormalizedType, Deps) ->
  case IsTopLevel of
    true ->
      {NormalizedType, Deps};
    false ->
      NewTypeName = generate_new_type(),
      NewType = t_userdef(NewTypeName),
      Dep = {NewTypeName, NormalizedType},
      true = ets:insert(Seen, {NormalizedType, NewType}),
      {NewType, [Dep|Deps]}
  end.

%% ----------------------------------------------------------------------------
%% Traverse a spec and substitute the local and remote types.
%% ----------------------------------------------------------------------------

-spec parse_spec(mfa(), stored_spec_value(), many_stored_types(), boolean()) -> erl_spec().
parse_spec(Mfa, Spec, ManyStoredTypes, NormalizeTypes) ->
  Conf = mk_conf(Mfa, ManyStoredTypes),
  {ParsedSpec, Deps} = parse_spec_clauses(Spec, Conf, []),
  true = cleanup_conf(Conf),
  %% TODO Maybe also normalize the parsed spec.
  case NormalizeTypes of
    false ->
      {ParsedSpec, Deps};
    true ->
      NormalizedDeps = normalize_type_deps(Deps),
      {ParsedSpec, NormalizedDeps}
  end.

parse_spec_clauses([], Conf, Acc) ->
  {lists:reverse(Acc), parse_type_deps(Conf)};
parse_spec_clauses([Clause|Clauses], Conf, Acc) ->
  {M,_,_} = getMfa(Conf),
  Simplified = simplify(Clause, M, new_env(), ordsets:new(), Conf),
  parse_spec_clauses(Clauses, Conf, [Simplified|Acc]).

parse_type_deps(Conf) ->
  case hasPendingTypes(Conf) of
    false -> getTypeDeps(Conf);
    true ->
      {Name, Fn} = next_pending_type(Conf),
      true = store_parsed_type(Conf, Name, Fn()),
      parse_type_deps(Conf)
  end.

bind_parameters([], [], Env) ->
  Env;
bind_parameters([P|Ps], [A|As], Env) ->
  bind_parameters(Ps, As, dict:store(P, A, Env)).

add_constraints([], Env) ->
  Env;
add_constraints([{Var, Type}|Cs], Env) ->
  F = fun(M, E, Visited, Conf) -> simplify(Type, M, E, Visited, Conf) end,
  Env1 = add_to_env(Var#t.rep, F, Env),
  add_constraints(Cs, Env1).

simplify_constraints(Constraints, CurrModule, Env, Visited, Conf) ->
  % Create the environment.
  Env1 = add_constraints(Constraints, Env),
  % Simplify the constraints.
  F = fun({Var, Type}, E) ->
      T = simplify(Type, CurrModule, Env1, Visited, Conf),
      add_to_env(Var#t.rep, T, E)
    end,
  lists:foldl(F, Env, Constraints).

-spec simplify(raw_type(), atom(), type_var_env(), ordsets:ordset(stored_spec_key()), parse_conf()) -> raw_type().
%% fun
simplify(#t{kind = ?function_tag, rep = {Params, Ret, Constraints}}=Raw, CurrModule, Env, Visited, Conf) ->
  Env1 = simplify_constraints(Constraints, CurrModule, Env, Visited, Conf),
  ParamsSimplified = [simplify(P, CurrModule, Env1, Visited, Conf) || P <- Params],
  RetSimplified = simplify(Ret, CurrModule, Env1, Visited, Conf),
  Rep = {ParamsSimplified, RetSimplified, []},
  Raw#t{rep = Rep};
simplify(#t{kind = ?function_tag, rep = {Ret, Constraints}}=Raw, CurrModule, Env, Visited, Conf) ->
  Env1 = simplify_constraints(Constraints, CurrModule, Env, Visited, Conf),
  RetSimplified = simplify(Ret, CurrModule, Env1, Visited, Conf),
  Rep = {RetSimplified, []},
  Raw#t{rep = Rep};
%% tuple
simplify(#t{kind = ?tuple_tag, rep = Types}=Raw, CurrModule, Env, Visited, Conf) ->
  Rep = [simplify(T, CurrModule, Env, Visited, Conf) || T <- Types],
  Raw#t{rep = Rep};
%% list / nonempty_list
simplify(#t{kind = Tag, rep = Type}=Raw, CurrModule, Env, Visited, Conf) when Tag =:= ?list_tag; Tag =:= ?nonempty_list_tag ->
  Rep = simplify(Type, CurrModule, Env, Visited, Conf),
  Raw#t{rep = Rep};
%% union
simplify(#t{kind = ?union_tag, rep = Types}=Raw, CurrModule, Env, Visited, Conf) ->
  Rep = [simplify(T, CurrModule, Env, Visited, Conf) || T <- Types],
  Raw#t{rep = Rep};
%% local type
simplify(#t{kind = ?local_tag, rep = {Name, Args}}, CurrModule, Env, Visited, Conf) ->
  Arity = length(Args),
  StoredTypes = getTypesCacheOfModule(CurrModule, Conf),
  case dict:find({type, Name, Arity}, StoredTypes) of
    error ->
      cuter_pp:error_retrieving_spec(getMfa(Conf), {unsupported_type, Name}),
      t_any();
    {ok, {Type, Params}} ->
      As = [simplify(A, CurrModule, Env, Visited, Conf) || A <- Args],
      Env1 = bind_parameters(Params, As, new_env()),
      Fn = fun() -> simplify(Type, CurrModule, Env1, Visited, Conf) end,
      % Add the type to the pending types, if needed.
      UniqueTypeName = unique_type_name(CurrModule, Name, As),
      true = add_pending_type(Conf, UniqueTypeName, Fn),
      % Return the reference to the type.
      t_userdef(UniqueTypeName)
  end;
%% type variable
simplify(#t{kind = ?type_variable, rep = TVar}=T, CurrModule, Env, Visited, Conf) ->
  case is_tvar_wild_card(T) of
    true -> t_any();
    false ->
      Key = {?type_variable, TVar},
      case ordsets:is_element(Key, Visited) of
        true ->
          cuter_pp:error_retrieving_spec(getMfa(Conf), {recursive_type, TVar}),
          t_any();
        false ->
          % FIXME dict:fetch/3 may fail if it's a free variable so let's generalize it to any().
          case lookup_in_env(TVar, Env) of
            V when is_function(V) ->
              Visited1 = ordsets:add_element(Key, Visited),
              V(CurrModule, Env, Visited1, Conf);
            V -> V
          end
        end
  end;
%% remote type
simplify(#t{kind = ?remote_tag, rep = {Module, Name, Args}}, CurrModule, Env, Visited, Conf) ->
  Arity = length(Args),
  Remote = {Module, Name, Arity},
  case ordsets:is_element(Remote, Visited) of
    true ->
      cuter_pp:error_retrieving_spec(getMfa(Conf), {recursive_type, Remote}),
      t_any();
    false ->
      StoredTypes = getTypesCacheOfModule(Module, Conf),
      case dict:find({type, Name, Arity}, StoredTypes) of
        error ->
          cuter_pp:error_retrieving_spec(getMfa(Conf), {could_not_access, Remote}),
          t_any();
        {ok, {Type, Params}} ->
          Visited1 = ordsets:add_element(Remote, Visited),
          As = [simplify(A, CurrModule, Env, Visited1, Conf) || A <- Args],
          Env1 = bind_parameters(Params, As, new_env()),
          Fn = fun() -> simplify(Type, Module, Env1, Visited1, Conf) end,
          % Add the type to the pending types, if needed.
          UniqueTypeName = unique_type_name(Module, Name, As),
          true = add_pending_type(Conf, UniqueTypeName, Fn),
          t_userdef(UniqueTypeName)
      end
  end;
%% record
simplify(#t{kind = ?record_tag, rep = {Name, OverridenFields}}, CurrModule, Env, Visited, Conf) ->
  StoredTypes = getTypesCacheOfModule(CurrModule, Conf),
  RecordDecl = dict:fetch({record, Name}, StoredTypes),
  Fields = fields_of_t_record(RecordDecl),
  ActualFields = replace_record_fields(Fields, OverridenFields),
  FinalFields = [{N, simplify(T, CurrModule, Env, Visited, Conf)} || {N, T} <- ActualFields],
  Simplified = [T || {_, T} <- FinalFields],
  t_tuple([t_atom_lit(Name)|Simplified]);
%% all others
simplify(Raw, _CurrModule, _Env, _Visited, _Conf) ->
  Raw.

-spec replace_record_fields([record_field_type()], [record_field_type()]) -> [record_field_type()].
replace_record_fields(Fields, []) ->
  Fields;
replace_record_fields(Fields, [{Name, Type}|Rest]) ->
  Replaced = lists:keyreplace(Name, 1, Fields, {Name, Type}),
  replace_record_fields(Replaced, Rest).

%% API for manipulating environments.
%% An environment is a mapping of type variables to their actual values.

new_env() ->
  dict:new().

add_to_env(Key, Value, Env) ->
  dict:store(Key, Value, Env).

lookup_in_env(Key, Env) ->
  case dict:find(Key, Env) of
    {ok, Value} -> Value;
    error -> t_any() % An unbound type variable.
  end.

%% ============================================================================
%% Traverse a pre-processed spec or type and find its remote dependencies, aka
%% the remote types that it contains.
%% ============================================================================

%% Finds the remote dependencies of a spec.
-spec find_remote_deps_of_spec(stored_spec_value(), stored_types()) -> [mfa()].
find_remote_deps_of_spec(Spec, LocalTypesCache) ->
  Deps = ordsets:union([find_remote_deps_of_type(Clause, LocalTypesCache) || Clause <- Spec]),
  ordsets:to_list(Deps).

%% Finds the remote dependencies of a type.
-spec find_remote_deps_of_type(raw_type(), stored_types()) -> [mfa()].
find_remote_deps_of_type(Type, LocalTypesCache) ->
  Deps = find_remote_deps(Type, ordsets:new(), ordsets:new(), LocalTypesCache),
  ordsets:to_list(Deps).

%% ----------------------------------------------------------------------------
%% Traverse a type and collect its remote dependencies.
%% Performing case analysis on the type of the type node.
%% ----------------------------------------------------------------------------

%% fun
find_remote_deps(#t{kind = ?function_tag, rep = {Params, Ret, Constraints}}, Visited, Deps, StoredTypes) ->
  Deps1 = lists:foldl(
    fun({_Var, Type}, Acc) -> find_remote_deps(Type, Visited, Acc, StoredTypes) end,
    Deps, Constraints
  ),
  Deps2 = lists:foldl(
    fun(P, Acc) -> find_remote_deps(P, Visited, Acc, StoredTypes) end,
    Deps1, Params
  ),
  find_remote_deps(Ret, Visited, Deps2, StoredTypes);
%% tuple or union
find_remote_deps(#t{kind = Tag, rep = Types}, Visited, Deps, StoredTypes) when Tag =:= ?tuple_tag; Tag =:= ?union_tag ->
  lists:foldl(fun(T, Acc) -> find_remote_deps(T, Visited, Acc, StoredTypes) end, Deps, Types);
%% list / nonempty_list
find_remote_deps(#t{kind = Tag, rep = Type}, Visited, Deps, StoredTypes) when Tag =:= ?list_tag; Tag =:= ?nonempty_list_tag ->
  find_remote_deps(Type, Visited, Deps, StoredTypes);
%% local type
find_remote_deps(#t{kind = ?local_tag, rep = {Name, Args}}, Visited, Deps, StoredTypes) ->
  Arity = length(Args),
  Local = {Name, Arity},
  case ordsets:is_element(Local, Visited) of
    true ->
      Deps;
    false ->
      case dict:find({type, Name, Arity}, StoredTypes) of
        error ->
          %% TODO Report that we cannot access the definition of the type.
          Deps;
        {ok, {Type, _Params}} ->
          Visited1 = ordsets:add_element(Local, Visited),
          Deps1 = lists:foldl(
            fun(Arg, Acc) -> find_remote_deps(Arg, Visited1, Acc, StoredTypes) end,
            Deps, Args
          ),
          find_remote_deps(Type, Visited1, Deps1, StoredTypes)
      end
  end;
%% type variable
find_remote_deps(#t{kind = ?type_variable}, _Visited, Deps, _StoredTypes) ->
  Deps;
%% remote type
find_remote_deps(#t{kind = ?remote_tag, rep = {Module, Name, Args}}, Visited, Deps, StoredTypes) ->
  Deps1 = lists:foldl(
    fun(Arg, Acc) -> find_remote_deps(Arg, Visited, Acc, StoredTypes) end,
    Deps, Args
  ),
  Remote = {Module, Name, length(Args)},
  ordsets:add_element(Remote, Deps1);
%% record
find_remote_deps(#t{kind = ?record_tag, rep = {Name, OverridenFields}}, Visited, Deps, StoredTypes) ->
  RecordDecl = dict:fetch({record, Name}, StoredTypes),
  Fields = fields_of_t_record(RecordDecl),
  ActualFields = replace_record_fields(Fields, OverridenFields),
  lists:foldl(
    fun({_N, T}, Acc) -> find_remote_deps(T, Visited, Acc, StoredTypes) end,
    Deps, ActualFields
  );
%% all others
find_remote_deps(_Type, _Visited, Deps, _StoredTypes) ->
  Deps.

%% ----------------------------------------------------------------------------
%% API for erl_type_defs().
%% ----------------------------------------------------------------------------

-spec erl_type_deps_map(fun((erl_type_dep()) -> T), erl_type_deps()) -> [T].
erl_type_deps_map(Fn, Deps) ->
  lists:map(Fn, Deps).

-spec get_type_name_from_type_dep(erl_type_dep()) -> string().
get_type_name_from_type_dep({Name, _Type}) ->
  Name.

-spec get_type_from_type_dep(erl_type_dep()) -> erl_type().
get_type_from_type_dep({_Name, Type}) ->
  Type.
