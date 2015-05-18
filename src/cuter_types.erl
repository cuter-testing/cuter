%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types).

-export([parse_spec/1, retrieve_types/1]).

-export_type([erl_type/0, erl_spec_clause/0, erl_spec/0]).

-include("include/cuter_macros.hrl").

%% Unified representation of Erlang types and specs
-type erl_type() :: any
                  | atom
                  | {atom, atom()}
                  | boolean
                  | float
                  | integer
                  | {integer, integer()}
                  | {list, erl_type()}
                  | {nonempty_list, erl_type()}
                  | nil
                  | {range, integer() | inf, integer() | inf}
                  | tuple
                  | {tuple, [erl_type()]}
                  | {union, [erl_type()]}.

-type erl_spec_clause() :: {[erl_type()], erl_type()}.
-type erl_spec() :: [erl_spec_clause(), ...].


%% Define tags
-define(simple_type, simt).
-define(local_type, loct).
-define(remote_type, remt).
-define(type_variable, vart).
-define(record_type, rect).
-define(type_var, tvar).
-define(atom_lit_tag, atom).
-define(integer_lit_tag, integer).
-define(integer_tag, integer).
-define(nil_tag, nil).
-define(any_tag, any).
-define(atom_tag, atom).
-define(float_tag, float).
-define(tuple_tag, tuple).
-define(list_tag, list).
-define(nonempty_list_tag, nonempty_list).
-define(union_tag, union).
-define(range_tag, range).
-define(bitstring_tag, bitstring).
-define(neg_inf, inf).
-define(pos_inf, inf).
-define(max_char, 16#10ffff).
-define(remote_tag, remote_type).
-define(local_tag, local).
-define(record_tag, record).

%% Pre-processed types.

-type type_name() :: atom().
-type type_arity() :: byte().
-type type_var() :: {?type_var, atom()}.
-type remote_type() :: {module(), type_name(), type_arity()}.
-type record_name() :: atom().
-type record_field_name() :: atom().
-type record_field_type() :: {record_field_name(), raw_type()}.
-type dep() :: remote_type().
-type deps() :: ordsets:set(remote_type()).
-record(t, {
  kind = ?simple_type,
  rep,
  deps = ordsets:new() :: deps()
}).
-type raw_type() :: t_any()               % any()
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
                  | t_local()             % Local Type Usage
                  | t_remote()            % Remote Type Usage
                  | t_record()            % Record Usage
                  | t_type_var()          % Type Variable
                  .

-type t_any() :: #t{kind :: ?simple_type, rep :: ?any_tag}.
-type t_nil() :: #t{kind :: ?simple_type, rep :: ?nil_tag}.
-type t_atom() :: #t{kind :: ?simple_type, rep :: ?atom_tag}.
-type t_atom_lit() :: #t{kind :: ?simple_type, rep :: {?atom_lit_tag, atom()}}.
-type t_integer() :: #t{kind :: ?simple_type, rep :: ?integer_tag}.
-type t_integer_lit() :: #t{kind :: ?simple_type, rep :: {?integer_lit_tag, integer()}}.
-type t_float() :: #t{kind :: ?simple_type, rep :: ?float_tag}.
-type t_tuple() :: #t{kind :: ?simple_type, rep :: ?tuple_tag}                   % tuple()
                 | #t{kind :: ?simple_type, rep :: {?tuple_tag, [raw_type()]}}.  % {TList}
-type t_list() :: #t{kind :: ?simple_type, rep :: {?list_tag, raw_type()}}.
-type t_nonempty_list() :: #t{kind :: ?simple_type, rep :: {?nonempty_list_tag, raw_type()}}.
-type t_union() :: #t{kind :: ?simple_type, rep :: {?union_tag, [raw_type()]}}.
-type t_range() :: #t{kind :: ?simple_type, rep :: {?range_tag, t_range_limit(), t_range_limit()}}.
-type t_range_limit() :: t_integer_lit() | t_integer_inf().
-type t_integer_inf() :: t_integer_pos_inf() | t_integer_neg_inf().
-type t_integer_pos_inf() :: #t{kind :: ?simple_type, rep :: ?pos_inf}.
-type t_integer_neg_inf() :: #t{kind :: ?simple_type, rep :: ?neg_inf}.
-type t_bitstring() :: #t{kind :: ?simple_type, rep :: {?bitstring_tag, integer()}}.
-type t_local() :: #t{kind :: ?local_type, rep :: {?local_tag, type_name(), [raw_type()]}}.
-type t_remote() :: #t{kind :: ?remote_type, rep :: {?remote_tag, module(), type_name(), [raw_type()]}}.
-type t_record() :: #t{kind :: ?record_type, rep :: {?record_tag, record_name(), [record_field_type()]}}.
-type t_type_var() :: #t{kind :: ?type_variable, rep :: type_var()}.

%% How pre-processed types are stored.
-type stored_type_key() :: {record, record_name()} | {type, type_name(), type_arity()}.
-type stored_type_value() :: [record_field_type()] | {raw_type(), [type_var()]}.
-type stored_types() :: dict:dict(stored_type_key(), stored_type_value()).


%% Pre-process the type & record declarations of a module.
-spec retrieve_types([cuter_cerl:cerl_attr_type()]) -> stored_types().
retrieve_types(TypeAttrs) ->
  lists:foldl(fun process_type_attr/2, dict:new(), TypeAttrs).

-spec process_type_attr(cuter_cerl:cerl_recdef() | cuter_cerl:cerl_typedef(), stored_types()) -> stored_types().
%% Declaration of a record.
process_type_attr({{record, Name}, Fields, []}, Processed) ->
  Fs = [t_field_from_form(Field) || Field <- Fields],
  Record = t_record(Name, Fs),
  dict:store({record, Name}, Record, Processed);
%% Declaration of a type.
process_type_attr({Name, Repr, Vars}, Processed) ->
  Type = t_from_form(Repr),
  Vs = [{?type_var, Var} || {var, _, Var} <- Vars],
  dict:store({type, Name, length(Vs)}, {Type, Vs}, Processed).

%% The fields of a declared record.
-spec t_field_from_form(cuter_cerl:cerl_record_field()) -> record_field_type().
t_field_from_form({record_field, _, Name}) ->
  {Name, t_any()};
t_field_from_form({record_field, _, Name, _Default}) ->
  {Name, t_any()};
t_field_from_form({typed_record_field, {record_field, _, Name}, Type}) ->
  {Name, t_from_form(Type)};
t_field_from_form({typed_record_field, {record_field, _, Name, _Default}, Type}) ->
  {Name, t_from_form(Type)}.

%% Parse a type.

-spec t_from_form(cerl:cerl_type()) -> raw_type().
%% Erlang_Atom
t_from_form({atom, _, Atom}) ->
  t_atom_lit(Atom);
%% Erlang_Integer
t_from_form({integer, _, Integer}) ->
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
  t_atom();
%% float()
t_from_form({type, _, float, []}) ->
  t_float();
%% tuple()
t_from_form({type, _, tuple, any}) ->
  t_tuple();
%% {TList}
t_from_form({type, _, tuple, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_tuple(Ts);
%% list()
t_from_form({type, _, list, []}) ->
  t_list();
%% list(Type)
t_from_form({type, _, list, [Type]}) ->
  T = t_from_form(Type),
  t_list(T);
%% Type1 | ... | TypeN
t_from_form({type, _, union, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_union(Ts);
%% boolean()
t_from_form({type, _, boolean, []}) ->
  t_union([t_atom_lit(true), t_atom_lit(false)]);
%% number()
t_from_form({type, _, number, []}) ->
  t_union([t_integer(), t_float()]);
%% Erlang_Integer..Erlang_Integer
t_from_form({type, _, range, [{integer, _, I1}, {integer, _, I2}]}) ->
  t_range(t_integer_lit(I1), t_integer_lit(I2));
%% non_neg_integer()
t_from_form({type, _, non_neg_integer, []}) ->
  t_range(t_integer_lit(0), t_pos_inf());
%% pos_integer()
t_from_form({type, _, pos_integer, []}) ->
  t_range(t_integer_lit(1), t_pos_inf());
%% neg_integer()
t_from_form({type, _, neg_integer, []}) ->
  t_range(t_neg_inf(), t_integer_lit(-1));
%% char()
t_from_form({type, _, char, []}) ->
  t_char();
%% byte()
t_from_form({type, _, byte, []}) ->
  t_range(t_integer_lit(0), t_integer_lit(255));
%% string()
t_from_form({type, _, string, []}) ->
  t_list(t_char());
%% nonempty_list()
t_from_form({type, _, nonempty_list, []}) ->
  t_nonempty_list();
%% nonempty_list(Type)
t_from_form({type, _, nonempty_list, [Type]}) ->
  T = t_from_form(Type),
  t_nonempty_list(T);
%% binary()
t_from_form({type, _, binary, []}) ->
  t_bitstring(8);
%% bitstring()
t_from_form({type, _, bitstring, []}) ->
  t_bitstring(1);
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
%% local type
t_from_form({type, _, Name, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_local(Name, Ts);
%% Type Variable
t_from_form({var, _, Var}) ->
  t_var(Var).


-spec t_bound_field_from_form(cuter_cerl:cerl_type_record_field()) -> record_field_type().
%% Record Field.
t_bound_field_from_form({type, _, field_type, [{atom, _, Name}, Type]}) ->
  {Name, t_from_form(Type)}.


%% Type constructors.

-spec t_any() -> t_any().
t_any() ->
  #t{rep = ?any_tag}.

-spec t_atom_lit(atom()) -> t_atom_lit().
t_atom_lit(Atom) ->
  #t{rep = {?atom_lit_tag, Atom}}.

-spec t_atom() -> t_atom().
t_atom() ->
  #t{rep = ?atom_tag}.

-spec t_integer_lit(integer()) -> t_integer_lit().
t_integer_lit(Integer) ->
  #t{rep = {?integer_lit_tag, Integer}}.

-spec t_integer() -> t_integer().
t_integer() ->
  #t{rep = ?integer_tag}.

-spec t_range(t_range_limit(), t_range_limit()) -> t_range().
t_range(Int1, Int2) ->
  #t{rep = {?range_tag, Int1, Int2}}.

-spec t_pos_inf() -> t_integer_pos_inf().
t_pos_inf() ->
  #t{rep = ?pos_inf}.

-spec t_neg_inf() -> t_integer_neg_inf().
t_neg_inf() ->
  #t{rep = ?neg_inf}.

-spec t_char() -> t_range().
t_char() ->
  t_range(t_integer_lit(0), t_integer_lit(?max_char)).

-spec t_nil() -> t_nil().
t_nil() ->
  #t{rep = ?nil_tag}.

-spec t_float() -> t_float().
t_float() ->
  #t{rep = ?float_tag}.

-spec t_list() -> t_list().
t_list() ->
  #t{rep = {?list_tag, t_any()}}.

-spec t_list(raw_type()) -> t_list().
t_list(Type) ->
  #t{rep = {?list_tag, Type}, deps = get_deps(Type)}.

-spec t_nonempty_list() -> t_nonempty_list().
t_nonempty_list() ->
  #t{rep = {?nonempty_list_tag, t_any()}}.

-spec t_nonempty_list(raw_type()) -> t_nonempty_list().
t_nonempty_list(Type) ->
  #t{rep = {?nonempty_list_tag, Type}, deps = get_deps(Type)}.

-spec t_tuple() -> t_tuple().
t_tuple() ->
  #t{rep = ?tuple_tag}.

-spec t_tuple([raw_type()]) -> t_tuple().
t_tuple(Types) ->
  #t{rep = {?tuple_tag, Types}, deps = unify_deps(Types)}.

-spec t_union([raw_type()]) -> t_union().
t_union(Types) ->
  #t{rep = {?union_tag, Types}, deps = unify_deps(Types)}.

-spec t_local(type_name(), [raw_type()]) -> t_local().
t_local(Name, Types) ->
  Rep = {?local_tag, Name, Types},
  #t{kind = ?local_type, rep = Rep, deps = unify_deps(Types)}.

-spec t_remote(module(), type_name(), [raw_type()]) -> t_remote().
t_remote(Mod, Name, Types) ->
  Rep = {?remote_tag, Mod, Name, Types},
  Dep = {Mod, Name, length(Types)},
  #t{kind = ?remote_type, rep = Rep, deps = add_dep(Dep, unify_deps(Types))}.

-spec t_var(atom()) -> t_type_var().
t_var(Var) ->
  #t{kind = ?type_variable, rep = {?type_var, Var}}.

-spec t_record(record_name(), [record_field_type()]) -> t_record().
t_record(Name, Fields) ->
  Rep = {?record_tag, Name, Fields},
  Ts = [T || {_, T} <- Fields],
  #t{kind = ?record_type, rep = Rep, deps = unify_deps(Ts)}.

-spec t_bitstring(integer()) -> t_bitstring().
t_bitstring(N) ->
  #t{rep = {?bitstring_tag, N}}.

%% Helper functions for dependencies.

-spec get_deps(raw_type()) -> deps().
get_deps(Type) ->
  Type#t.deps.

-spec add_dep(dep(), deps()) -> deps().
add_dep(Dep, Deps) ->
  ordsets:add_element(Dep, Deps).

-spec unify_deps([raw_type()]) -> deps().
unify_deps(Types) ->
  ordsets:union([T#t.deps || T <- Types]).


%% TODO All the code under here will soon change.

%% Parse the spec of an MFA.

-spec parse_spec(cuter_cerl:cerl_spec()) -> erl_spec().
parse_spec(Spec) ->
  [parse_clause(S, orddict:new()) || S <- Spec].

-spec parse_clause(cuter_cerl:cerl_func() | cuter_cerl:cerl_bounded_func(), orddict:orddict()) -> erl_spec_clause().
parse_clause({type, _, 'fun', [Product, Ret]}, Bound) ->
  {type, _, product, Params} = Product,
  {[parse_type(P, Bound) || P <- Params], parse_type(Ret, Bound)};
parse_clause({type, _, bounded_fun, [Fun, Constraints]}, Bound) ->
  Bound1 = bind_constraints(Constraints, Bound),
  parse_clause(Fun, Bound1).

%% Store the constraints of a bounded fun.
bind_constraints(Constraints, Bound) ->
  lists:foldl(fun(Cst, Bnd) -> parse_constraint(Cst, Bnd) end, Bound, Constraints).

%% Store the type of a constraint (in a bounded fun).
parse_constraint({type, _, constraint, [{atom, _, is_subtype}, [Var, Type]]}, Bound) ->
  {var, _, V} = Var,
  orddict:store({var, V}, fun(Bnd) -> parse_type(Type, Bnd) end, Bound).


%% Parse an Erlang type.
-spec parse_type(cuter_cerl:cerl_type(), orddict:orddict()) -> erl_type().
parse_type({atom, _, Atom}, _Bound) ->
  {atom, Atom};
parse_type({integer, _, Integer}, _Bound) ->
  {integer, Integer};
parse_type({type, _, nil, []}, _Bound) ->
  nil;
parse_type({type, _, any, []}, _Bound) ->
  any;
parse_type({type, _, term, []}, _Bound) ->
  any;
parse_type({type, _, atom, []}, _Bound) ->
  atom;
parse_type({type, _, boolean, []}, _Bound) ->
  boolean;
parse_type({type, _, integer, []}, _Bound) ->
  integer;
parse_type({type, _, float, []}, _Bound) ->
  float;
parse_type({type, _, nonempty_list, []}, _Bound) ->
  {nonempty_list, any};
parse_type({type, _, nonempty_list, [Type]}, Bound) ->
  {nonempty_list, parse_type(Type, Bound)};
parse_type({type, _, list, []}, _Bound) ->
  {list, any};
parse_type({type, _, list, [Type]}, Bound) ->
  {list, parse_type(Type, Bound)};
parse_type({type, _, tuple, any}, _Bound) ->
  tuple;
parse_type({type, _, tuple, Types}, Bound) ->
  {tuple, [parse_type(T, Bound) || T <- Types]};
parse_type({type, _, union, Types}, Bound) ->
  {union, [parse_type(T, Bound) || T <- Types]};
parse_type({var, _, Var}, Bound) ->
  case orddict:find({var, Var}, Bound) of
    {ok, V} -> V(Bound);
    error -> any
  end;
parse_type({ann_type, _, [_Var, Type]}, Bound) ->
  parse_type(Type, Bound);
parse_type({paren_type, _, [Type]}, Bound) ->
  parse_type(Type, Bound);
%% More custom types.
parse_type({type, _, orddict, []}, _Bound) ->
  {list, {tuple, [any, any]}};
parse_type({type, _, number, []}, _Bound) ->
  {union, [integer, float]};
parse_type({type, _, char, []}, _Bound) ->
  {range, 0, 16#10ffff};
parse_type({type, _, string, []}, _Bound) ->
  {list, {range, 0, 16#10ffff}};
parse_type({type, _, range, [{integer, _, I1}, {integer, _, I2}]}, _Bound) ->
  {range, I1, I2};
parse_type({type, _, non_neg_integer, []}, _Bound) ->
  {range, 0, inf};
parse_type({type, _, pos_integer, []}, _Bound) ->
  {range, 1, inf};
parse_type({type, _, neg_integer, []}, _Bound) ->
  {range, inf, -1};
%% Unsupported type.
parse_type(Type, _Bound) -> throw({unsupported_type, Type}).








