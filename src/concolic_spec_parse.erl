%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_spec_parse).

%% 
%% Core Erlang AST #c_module{}.attrs
%% =================================
%% 
%% #c_module{}.attrs :: (Attr)*
%% 
%% Attr :: TypeAttrDef | SpecAttrDef
%% 
%% TypeAttrDef :: {{c_literal, _, type}, {c_literal, _, [TypeAttr]}}
%% │
%% └─ TypeAttr :: RecordDef | TypeDef
%%    │
%%    ├─ RecordDef :: {{record, RecordName :: atom()}, (RecordField)+, []}
%%    │  └─ RecordField :: UntypedRecordField | TypedRecordField
%%    │     ├─ UntypedRecordField :: {record_field, _Ln, FieldName} 
%%    │     │                      | {record_field, _Ln, FieldName, DefaultValue :: term()}
%%    │     └─ TypedRecordField :: {typed_record_field, UntypedRecordField, Type}
%%    │
%%    └─ TypeDef :: {TypeName :: atom(), Type, (FreeVar)*}
%%       └─ FreeVar :: {var, _Ln, VarName :: atom()}
%% 
%% SpecAttrDef :: {{c_literal, _, spec}, {c_literal, _, [SpecAttr]}}
%% │
%% └─ SpecAttr :: {{F :: atom(), A :: byte()}, [Spec]}
%%    ├─ Spec :: Fun | BoundedFun
%%    │
%%    ├─ Fun :: {type, _Ln, 'fun', [Product, Type]}
%%    │  └─ Product :: {type, _Ln, product, (Type)*}
%%    │
%%    └─ BoundedFun :: {type, _Ln, bounded_fun, [Fun, (Constraint)*]}
%%       └─ Constraint :: {type, _Ln, constraint, SubTypeCnst}
%%          └─ SubTypeCnst :: [{atom, _Ln, is_subtype}, [VarName :: atom(), Type]]
%% 
%% Type :: Fun
%%       | Literal
%%       | {type, _Ln, any, []}
%%       | {type, _Ln, term, []}
%%       | {type, _Ln, atom, []}
%%       | {type, _Ln, binary, []}
%%       | {type, _Ln, bitstring, []}
%%       | {type, _Ln, boolean, []}
%%       | {type, _Ln, byte, []}
%%       | {type, _Ln, char, []}
%%       | {type, _Ln, float, []}
%%       | {type, _Ln, integer, []}
%%       | {type, _Ln, list, (Type)*}
%%       | {type, _Ln, mfa, []}
%%       | {type, _Ln, module, []}
%%       | {type, _Ln, neg_integer, []}
%%       | {type, _Ln, node, []}
%%       | {type, _Ln, no_return, []}
%%       | {type, _Ln, non_neg_integer, []}
%%       | {type, _Ln, none, []}
%%       | {type, _Ln, nonempty_string, []}
%%       | {type, _Ln, number, []}
%%       | {type, _Ln, pid, []}
%%       | {type, _Ln, pos_integer, []}
%%       | {type, _Ln, range, [Type, Type]}
%%       | {type, _Ln, record, [{atom, _Ln, RecordName :: atom()}]}
%%       | {type, _Ln, string, []}
%%       | {type, _Ln, timeout, []}
%%       | {type, _Ln, tuple, [any] | (Type)+}
%%       | {type, _Ln, union, (Type)+}
%%       | {type, _Ln, UserDefinedType :: atom(), (Type)*}
%%       | {parent_type, _Ln, [Type]}
%%       | {ann_type, _Ln, [VarName :: atom(), Type]}
%%       | {remote_type, _Ln, RemType}
%%       | BoundedVar (when we have a BoundedFun or a UserDefinedType)
%%
%%  Literal :: {atom, _Ln, atom()} | {integer, _Ln, integer()} | {type, _Ln, nil, []}
%%  RemType :: [{atom, _Ln, M :: atom()}, {atom, _Ln, T :: atom()}, (Type)*]
%%  BoundedVar :: {var, _Ln, VarName :: atom()}
%% 

-export([retrieve_spec/2,
         parse_specs_in_file/1, locate_spec_in_file/2]).

-include_lib("compiler/src/core_parse.hrl").

-record(type, {
  line :: pos_integer(),
  name :: atom(),
  args :: any | list()
}).

-type empty() :: maybe_empty | non_empty.
-type type_sig() :: {literal, term()}
                  | any
                  | atom
                  | binary
                  | bitstring
                  | boolean
                  | byte
                  | char
                  | float
                  | {function, [type_sig()], type_sig()}
                  | {integer, any | pos | neg | non_neg}
                  | {list, empty(), [type_sig()]}
                  | mfa
                  | module
                  | node
                  | none
                  | number
                  | pid
                  | {range, type_sig(), type_sig()}
                  | {string, empty()}
                  | timeout
                  | {tuple, [type_sig()]}
                  | {union, [type_sig()]}.

%% =================================================
%% Debugging Funs
%% =================================================

generate_AST(File) ->
  {ok, M} = compile:file(File, [to_core]),
  Core = atom_to_list(M) ++ ".core",
  {ok, FileContents} = file:read_file(Core),
  Data = binary_to_list(FileContents),
  {ok, Tokens, _} = core_scan:string(Data),
  {ok, AST} = core_parse:parse(Tokens),
  AST.

spec_and_bind_types(File) ->
  AST = generate_AST(File),
  Attrs = AST#c_module.attrs,
  Types = lists:filtermap(fun filter_types/1, Attrs),
  Specs = lists:filtermap(fun filter_specs/1, Attrs),
  BoundTypes = declare_types(Types),
  {Specs, BoundTypes}.

-spec parse_specs_in_file(file:name()) -> ok.
parse_specs_in_file(File) ->
  {Specs, BoundTypes} = spec_and_bind_types(File),
  parse_all_specs(Specs, BoundTypes).

parse_all_specs(Specs, BoundTypes) ->
  F = fun(X) ->
    io:format("~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),
%    io:format("~p~n",[X]),
    try
      {{F, A}, T} = parse_spec(X, BoundTypes),
      io:format("Spec for ~p/~p~n", [F, A]),
      pp_sig(T),
      io:format("~n"),
      pp_sig(simplify(T))
    catch
      exit:E -> io:format("Parse Spec Exception: ~p~n", [E])
    end,
    io:format("~n")
  end,
  lists:foreach(F, Specs).

-spec locate_spec_in_file(mfa(), file:name()) -> {ok, type_sig()} | error.
locate_spec_in_file(MFA, File) ->
  {Specs, BoundTypes} = spec_and_bind_types(File),
  locate_mfa_spec(MFA, Specs, BoundTypes).

%% =================================================

-spec retrieve_spec(mfa(), [{cerl:cerl(), cerl:cerl()}]) -> {ok, type_sig()} | error.

retrieve_spec(MFA, Attrs) ->
  Types = lists:filtermap(fun filter_types/1, Attrs),
  Specs = lists:filtermap(fun filter_specs/1, Attrs),
  BoundTypes = declare_types(Types),
  locate_mfa_spec(MFA, Specs, BoundTypes).

locate_mfa_spec(_MFA, [], _BoundTypes) -> error;
locate_mfa_spec({_M, F, A}=MFA, [S|Ss], BoundTypes) ->
  case parse_spec(S, BoundTypes) of
    {{F, A}, T} -> {ok, simplify(T)};
    _ -> locate_mfa_spec(MFA, Ss, BoundTypes)
  end.

filter_specs({#c_literal{val = spec}, #c_literal{val = [Spec]}}) -> {true, Spec};
filter_specs(_Attr) -> false.

filter_types({#c_literal{val = type}, #c_literal{val = [Type]}}) -> {true, Type};
filter_types(_Attr) -> false.

%% Replace unions that have an any clause with any
%% (also nested unions)
-spec simplify(type_sig()) -> type_sig().

simplify({function, Ps, R}) ->
  {function, lists:map(fun simplify/1, Ps), simplify(R)};
simplify({list, Empty, Vs}) ->
  {list, Empty, lists:map(fun simplify/1, Vs)};
simplify({range, From, To}) ->
  {range, simplify(From), simplify(To)};
simplify({tuple, Vs}) ->
  {tuple, lists:map(fun simplify/1, Vs)};
simplify({union, Vs}) ->
  SimpVs = lists:map(fun simplify/1, Vs),
  case lists:any(fun(T) -> T =:= any end, SimpVs) of
    true  -> any;
    false -> {union, SimpVs}
  end;
simplify(T) -> T.

%% -------------------------------------------------
%% Declare Types
%% -------------------------------------------------

declare_types(Types) ->
  F = fun(X, Y) -> 
%    io:format("~n$$$~n~p~n$$$~n", [X]),
    declare_type(X, Y)
  end,
  lists:foldl(F, orddict:new(), Types).

%% Store the declaration of a record
declare_type({{record, RecordName}, RecordSig, []}, Bound) ->
  F = fun(BX) ->
    Ts = lists:map(fun(X) -> parse_record_field(X, BX) end, RecordSig),
    {tuple, [{literal, RecordName} | Ts]}
  end,
  orddict:store({record, RecordName}, F, Bound);
%% Store the declaration of a type
declare_type({Name, TypeSig, Vars}, Bound) ->
  Vs = lists:map(fun({var, _Ln, X}) -> {var, X} end, Vars),
  F = fun(As, BX) ->
    Ts = lists:map(fun(X) -> fun(T) -> parse_type(X, T) end end, As),
    Zs = lists:zip(Vs, Ts),
    BX1 = lists:foldl(fun({A, B}, C) -> orddict:store(A, B, C) end, BX, Zs),
    parse_type(TypeSig, BX1)
  end,
  orddict:store({type, {Name, length(Vars)}}, F, Bound);
%% XXX Do not expect to get here
declare_type(Type, _) -> exit({unknown_type, Type}).

%% Get the type of a record's field
parse_record_field({record_field, _Ln, _FieldName}, _Bound) ->
  any;
parse_record_field({record_field, _Ln, _FieldName, _DefVal}, _Bound) ->
  any;
parse_record_field({typed_record_field, {record_field, _Ln, _FieldName}, Type}, Bound) ->
  parse_type(Type, Bound);
parse_record_field({typed_record_field, {record_field, _Ln, _FieldName, _DefVal}, Type}, Bound) ->
  parse_type(Type, Bound).

%% -------------------------------------------------
%% Parse a Spec
%% -------------------------------------------------

parse_spec({FA, [Type]}, Bound) ->
  {FA, parse_type(Type, Bound)}.

%% literals (atom, integer, [])
parse_type({atom, _, A}, _Bound) -> {literal, A};
parse_type({integer, _, I}, _Bound) -> {literal, I};
parse_type(#type{name = nil, args = []}, _Bound) -> {literal, nil};
%% any(), term()
parse_type(#type{name = X, args = []}, _Bound) when X =:= any; X =:= term->
  any;
%% atom(), binary(), bitstring(), boolean(), byte(), char()
%% float(), mfa(), module(), node(), number(), pid(), timeout()
parse_type(#type{name = X, args = []}, _Bound)
  when X =:= atom;
       X =:= binary;
       X =:= bitstring;
       X =:= boolean;
       X =:= byte;
       X =:= char;
       X =:= float;
       X =:= mfa;
       X =:= module;
       X =:= node;
       X =:= number;
       X =:= pid;
       X =:= timeout ->
  X;
%% integer()
parse_type(#type{name = integer, args = []}, _Bound) ->
  {integer, any};
parse_type(#type{name = pos_integer, args = []}, _Bound) ->
  {integer, pos};
parse_type(#type{name = neg_integer, args = []}, _Bound) ->
  {integer, neg};
parse_type(#type{name = non_neg_integer, args = []}, _Bound) ->
  {integer, non_neg};
%% none(), no_return()
parse_type(#type{name = X, args = []}, _Bound) when X =:= none; X =:= no_return ->
  none;
%% string()
parse_type(#type{name = string, args = []}, _Bound) -> {string, maybe_empty};
parse_type(#type{name = nonempty_string, args = []}, _Bound) -> {string, non_empty};
%% list()
parse_type(#type{name = L, args = Args}, Bound) when L =:= list; L =:= nonempty_list ->
  Ts = lists:map(fun(X) -> parse_type(X, Bound) end, Args),
  case L of
    list -> {list, maybe_empty, Ts};
    nonempty_list -> {list, non_empty, Ts}
  end;
%% tuple()
parse_type(#type{name = tuple, args = any}, _Bound) ->
  {tuple, []};
parse_type(#type{name = tuple, args = Args}, Bound) ->
  Ts = lists:map(fun(X) -> parse_type(X, Bound) end, Args),
  {tuple, Ts};
%% union()
parse_type(#type{name = union, args = Args}, Bound) ->
  Ts = lists:map(fun(X) -> parse_type(X, Bound) end, Args),
  {union, Ts};
%% range()
parse_type(#type{name = range, args = [From, To]}, Bound) ->
  {range, parse_type(From, Bound), parse_type(To, Bound)};
%% bounded_fun
parse_type(#type{name = bounded_fun, args = [Fun, Cs]}, Bound) ->
  Bound1 = bind_constraints(Cs, Bound),
  parse_type(Fun, Bound1);
%% fun
parse_type(#type{name = 'fun', args = [Param, Result]}, Bound) ->
  Ptype = ensure_param_list(parse_type(Param, Bound)),
  Rtype = parse_type(Result, Bound),
  {function, Ptype, Rtype};
%% product (used in fun)
parse_type(#type{name = product, args = Args}, Bound) ->
  lists:map(fun(X) -> parse_type(X, Bound) end, Args);
%% annotated type
parse_type({ann_type, _Ln, [_Var, Type]}, Bound) ->
  parse_type(Type, Bound);
%% parenthesized type
parse_type({paren_type, _Ln, [Type]}, Bound) ->
  parse_type(Type, Bound);
%% record()
parse_type(#type{name = record, args = [{atom, _Ln, Rec}]}, Bound) ->
  V = orddict:fetch({record, Rec}, Bound),
  V(Bound);
%% user defined type
parse_type(#type{name = Type, args = Args}, Bound) ->
  V = orddict:fetch({type, {Type, length(Args)}}, Bound),
  V(Args, Bound);
%% bound variable (used in bounded_funs, records, user defined types)
parse_type({var, _Ln, Var}, Bound) ->
  V = orddict:fetch({var, Var}, Bound),
  V(Bound);
%% XXX
parse_type(Type, _) -> exit(Type).

%% Store the constraints of a bounded fun
bind_constraints(Cs, Bound) ->
  lists:foldl(fun(X, Y) -> parse_constraint(X, Y) end, Bound, Cs).

%% Store the type of a constraint (in a bounded fun)
parse_constraint(#type{name = constraint, args = [{atom, _, is_subtype}, [Var, Type]]}, Bound) ->
  {var, _, V} = Var,
  orddict:store({var, V}, fun(BX) -> parse_type(Type, BX) end, Bound).

ensure_param_list(Ps) when is_list(Ps) -> Ps;
ensure_param_list(P) -> [P].

%% -------------------------------------------------
%% Pretty Print a type_sig()
%% -------------------------------------------------

%% literal
pp_sig({literal, Lit}) -> io:format("~p", [Lit]);
%% any, atom, binary, bitstring, boolean, byte, char,
%% float, mfa, module, node, none, number, pid, timeout
pp_sig(T) when T =:= any;
               T =:= atom;
               T =:= binary;
               T =:= bitstring;
               T =:= boolean;
               T =:= byte;
               T =:= char;
               T =:= float;
               T =:= mfa;
               T =:= module;
               T =:= node;
               T =:= none;
               T =:= number;
               T =:= pid;
               T =:= timeout ->
  io:format("~p()", [T]);
%% string
pp_sig({string, maybe_empty}) -> io:format("string()");
pp_sig({string, non_empty})   -> io:format("nonempty_string()");
%% integer
pp_sig({integer, any}) -> io:format("integer()");
pp_sig({integer, X})   -> io:format("~p_integer()", [X]);
%% list
pp_sig({list, maybe_empty, []}) -> io:format("[any()]");
pp_sig({list, non_empty, []})   -> io:format("[any(), ...]");
pp_sig({list, Empty, [S|Sigs]}) ->
  io:format("["),
  pp_sig(S),
  lists:foreach(fun(X) -> io:format(" | "), pp_sig(X) end, Sigs),
  case Empty of
    maybe_empty -> ok;
    non_empty -> io:format(", ...")
  end,
  io:format("]");
%% union
pp_sig({union, [S|Sigs]}) ->
  io:format("("),
  pp_sig(S),
  lists:foreach(fun(X) -> io:format(" | "), pp_sig(X) end, Sigs),
  io:format(")");
%% range
pp_sig({range, F, T}) ->
  pp_sig(F),
  io:format(".."),
  pp_sig(T);
%% tuple
pp_sig({tuple, []}) -> io:format("tuple()");
pp_sig({tuple, [S|Sigs]}) ->
  io:format("{"),
  pp_sig(S),
  lists:foreach(fun(X) -> io:format(" , "), pp_sig(X) end, Sigs),
  io:format("}");
%% fun
pp_sig({function, [], R}) ->
  io:format("fun() -> "),
  pp_sig(R);
pp_sig({function, [P|Ps], R}) ->
  io:format("fun("),
  pp_sig(P),
  lists:foreach(fun(X) -> io:format(" , "), pp_sig(X) end, Ps),
  io:format(") -> "),
  pp_sig(R).

