-module(cuter_maybe_error_annotation).
-export([preprocess/3, preprocess/4, get_force_constraint_logging/1, get_maybe_error_bin/2, get_maybe_error_bin_anno/2, get_distrust_type_dependent/1]).
-export_type([maybe_error/0, symbol_table/0]).

%% =====
%% types
%% =====

-type maybe_error() :: false | type_dependent | true.
-type symbol_table() :: dict:dict().

%% ============================
%% annotating a callgraph logic
%% ============================

-spec st_from_tsm() -> dict:dict().
st_from_tsm() ->
  lists:foldl(
    fun({Fun, _}, ST) ->
	dict:store(Fun, {type_dependent, 'fun'}, ST)
    end,
    dict:new(),
    dict:to_list(cuter_type_dependent_functions:original_tsm())
   ).

-spec annotate_callgraph(cuter_graphs:graph_node(), dict:dict(), cuter_graphs:graph(), boolean()) -> dict:dict().
annotate_callgraph(EntryPoint, FunctionAsts, Graph, CheckTypes) ->
  {Annotated, _} = 
    case CheckTypes of
      false -> annotate_callgraph(EntryPoint, FunctionAsts, Graph, dict:new(), CheckTypes);
      true -> annotate_callgraph(EntryPoint, FunctionAsts, Graph, st_from_tsm(), CheckTypes)
    end,
  Annotated.

-spec annotate_callgraph(cuter_graphs:graph_node(), dict:dict(), cuter_graphs:graph(), symbol_table(), boolean()) -> {dict:dict(), symbol_table()}.
annotate_callgraph(Node, FunctionAsts, Graph, ST, CheckTypes) ->
  {FunctionAsts1, ST1} = lists:foldl(fun(A, {Funs, SmT}) -> annotate_callgraph(A, Funs, Graph, SmT, CheckTypes) end, {FunctionAsts, ST}, cuter_graphs:children(Node, Graph)),
  case Node of
    {node, Name} ->
      {ok, PrevAST} = dict:find(Name, FunctionAsts1),
      {NewAST, _C, _SelfReffed} = annotate_maybe_error(PrevAST, ST1, sets:new(), element(1, Name), CheckTypes),
      {dict:store(Name, NewAST, FunctionAsts1), dict:store(Name, {get_maybe_error(NewAST), 'fun'}, ST1)};
    {cycle, Cycle} ->
      cycle_annotation(Cycle, FunctionAsts1, ST1, CheckTypes)
  end.

cycle_annotation(Cycle, FunctionAsts, ST, CheckTypes) ->
  ASTS = [element(2, dict:find(A, FunctionAsts)) || A <- Cycle],
  CycleSet = sets:from_list(Cycle),
  {NewASTS, NewST} = cycle_annotation_helper(Cycle, ASTS, ST, CycleSet, CheckTypes),
  {
   lists:foldl(
     fun({Name, AST}, Y) -> dict:store(Name, AST, Y) end,
     FunctionAsts, 
     lists:zip(Cycle, NewASTS)
    ),
   NewST
  }.

cycle_annotation_helper(Cycle, ASTS, ST, CycleSet, CheckTypes) ->
  {NewASTS, ST1, C} = cycle_pass(Cycle, ASTS, ST, CycleSet, CheckTypes),
  case C of
    false ->
      {NewASTS, ST1};
    true ->
      cycle_annotation_helper(Cycle, NewASTS, ST1, CycleSet, CheckTypes)
  end.

cycle_pass(Cycle, ASTS, ST, CycleSet, CheckTypes) ->
  cycle_pass_helper(CycleSet, Cycle, ASTS, ST, [], false, CheckTypes).

cycle_pass_helper(_, [], _, ST, AccAST, AccC, _) -> {lists:reverse(AccAST), ST, AccC};
cycle_pass_helper(CycleSet, [Name|Names], [AST|ASTS], ST, AccAST, AccC, CheckTypes) ->
  {NewAST, C, IgnoredFound} = annotate_maybe_error(AST, ST, CycleSet, element(1, Name), CheckTypes),
  ST1 = dict:store(Name, {get_maybe_error(NewAST), 'fun'}, ST),
  cycle_pass_helper(CycleSet, Names, ASTS, ST1, [NewAST|AccAST], AccC or C or IgnoredFound, CheckTypes).


%% ===========================
%% annotating a function logic
%% ===========================

-spec update_ann(cerl:cerl(), maybe_error()) -> cerl:cerl().
update_ann(T, Maybe_Error) ->
  Anno = cerl:get_ann(T),
  cerl:set_ann(T, update_ann(Anno, Maybe_Error, [], false)).

-spec update_ann([any()], maybe_error(), [any()], atom()) -> [any()].
update_ann([], Maybe_Error, Acc, false) -> [{maybe_error, Maybe_Error}|Acc];
update_ann([], _, Acc, true) -> Acc; 
update_ann([{maybe_error, _}|T], Maybe_Error, Acc, _) -> update_ann(T, Maybe_Error, [{maybe_error, Maybe_Error}|Acc], true); 
update_ann([H|T], Maybe_Error, Acc, Found) -> update_ann(T, Maybe_Error, [H|Acc], Found).

-spec add_force_constraint_logging(cerl:cerl()) -> cerl:cerl().
add_force_constraint_logging(Tree) ->
  Anno = cerl:get_ann(Tree),
  case cuter_graphs:list_contains({force_constraint_logging, true}, Anno) of
    true -> Tree;
    false -> cerl:add_ann([{force_constraint_logging, true}], Tree)
  end.

add_distrust_type_dependent(Tree) ->
  Anno = cerl:get_ann(Tree),
  case cuter_graphs:list_contains({distrust_type_dependent, true}, Anno) of
    true -> Tree;
    false -> cerl:add_ann([{distrust_type_dependent, true}], Tree)
  end.

put_vars(Vars, Flags, SM) ->
  lists:foldl(fun({Var, Flag}, B) -> dict:store(cerl:var_name(Var), Flag, B) end, SM, lists:zip(Vars, Flags)).

annotate_maybe_error(AST, ST, Ignored, Mod, CheckTypes) ->
  {NewAST, C, _, IgnoredCall} = annotate_maybe_error(AST, ST, false, Ignored, Mod, CheckTypes),
  {NewAST, C, IgnoredCall}.

-spec annotate_maybe_error(cerl:cerl(), symbol_table(), boolean(), sets:set(), module(), boolean()) -> {cerl:cerl(), boolean(), sets:set(), boolean()}.
annotate_maybe_error(Tree, SM, Force, Ignored, Mod, CheckTypes) ->
  CurMaybe_Error = get_maybe_error(Tree),
  case cerl:type(Tree) of
%    alias ->
    'apply' ->
      Op = cerl:apply_op(Tree),
      {Op1, C1, IgnoreFound1} = 
	case cerl:type(Op) of
	  var ->
	    case cerl:var_name(Op) of
	      {F, A} ->
		case dict:find({Mod, F, A}, SM) of
		  {ok, {Value, 'fun'}} ->
		    case Value of
		      type_dependent when CheckTypes ->
			case cuter_spec_checker:get_cerl_type(Tree) of
			  notype -> {update_ann(Op, true), true =/= CurMaybe_Error, false};
			  _ -> {update_ann(Op, type_dependent), type_dependent =/= CurMaybe_Error, false}
			end;
		      _ -> {update_ann(Op, Value), Value =/= CurMaybe_Error, false}
		    end;
		  _ ->
		    case dict:find({F, A}, SM) of
		      {ok, {Value, FunType}} when FunType =:= 'fun' orelse FunType =:= letvar ->
			case Value of
			  type_dependent when CheckTypes ->
			    case cuter_spec_checker:get_cerl_type(Tree) of
			      notype -> {update_ann(Op, true), true =/= CurMaybe_Error, false};
			      _ -> {update_ann(Op, type_dependent), type_dependent =/= CurMaybe_Error, false}
			    end;
			  _ -> 
			    {update_ann(Op, Value), Value =/= CurMaybe_Error, false}
			end;
		      _ ->
			case sets:is_element({Mod, F, A}, Ignored) of
			  false ->
			    {update_ann(Op, true), true =/= CurMaybe_Error, false};
			  true ->
			    {update_ann(Op, false), true =/= CurMaybe_Error, true}
			end
		    end
		end;
	      Name ->
		case dict:find(Name, SM) of
		  {ok, {Value, _FunType}} -> %when FunType =:= 'fun' orelse FunType =:= letvar ->
		    case Value of
		      type_dependent when CheckTypes ->
			case cuter_spec_checker:get_cerl_type(Tree) of
			  notype -> {update_ann(Op, true), true =/= CurMaybe_Error, false};
			  _ -> {update_ann(Op, type_dependent), type_dependent =/= CurMaybe_Error, false}
			end;
		      _ -> 
			{update_ann(Op, Value), Value =/= CurMaybe_Error, false}
		    end;
		  _ ->
		    {update_ann(Op, true), true =/= CurMaybe_Error, false}
		end
	    end;
	  _ ->
	    error("unhandled op")
	end,
      {Args, C2, Found, IgnoreFound2} = annotate_maybe_error_all(cerl:apply_args(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = maybe_error_or([get_maybe_error(Op1), get_all_maybe_error(Args)]),
      case get_all_maybe_error(Args) of 
	true ->
	  Tree1 = add_distrust_type_dependent(Tree);
	_ -> 
	  Tree1 = Tree
      end,
      {cerl:update_c_apply(update_ann(Tree1, NewMaybe_Error), Op1, Args), C1 or C2, Found, IgnoreFound1 or IgnoreFound2};
%    binary -> meta
%    bitstr -> meta
    call ->
      ModName = cerl:call_module(Tree),
      Name = cerl:call_name(Tree),
      Arity = length(cerl:call_args(Tree)),
      {NewAnn, IgnoreFound1} = 
	case cerl:is_literal(ModName) andalso cerl:is_literal(Name) of
	  true ->
	    case dict:find({element(3, ModName), element(3, Name), Arity}, SM) of
	      {ok, {Value, 'fun'}} ->
		case Value of
		  type_dependent when CheckTypes ->
		    case cuter_spec_checker:get_cerl_type(Tree) of
		      notype -> {true,  false};
		      _ -> {type_dependent, false}
		    end;
		  _ -> {Value, false}
		end;
	      _ ->
		case sets:is_element({element(3, ModName), element(3, Name), Arity}, Ignored) of
		  false ->
		    {true, false};
		  true ->
		    {true, true}
		end
	    end;
	  _ -> throw("Unsupported call")
	end,
      {Args, C1, Found, IgnoreFound2} = annotate_maybe_error_all(cerl:call_args(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = maybe_error_or([NewAnn, get_all_maybe_error(Args)]),
      C2 = NewMaybe_Error =/= CurMaybe_Error,
      case get_all_maybe_error(Args) of
	true ->
	  Tree1 = add_distrust_type_dependent(Tree);
	_ -> 
	  Tree1 = Tree
      end,
      {cerl:update_c_call(update_ann(Tree1, NewMaybe_Error), ModName, Name, Args), C1 or C2, Found, IgnoreFound1 or IgnoreFound2};
    'case' ->
      {Clauses, C1, Found1, IgnoreFound1} = annotate_maybe_error_all(cerl:case_clauses(Tree), SM, Force, Ignored, Mod, CheckTypes),
      ClausesError1 = get_all_maybe_error(Clauses),
      ClausesError =
	case unreachable_clauses(Clauses) of
	  true -> maybe_error_or([ClausesError1, type_dependent]);
	  false -> ClausesError1
	end,
      {Arg, C2, Found2, IgnoreFound2} = 
	case ClausesError of
	  true -> annotate_maybe_error(cerl:case_arg(Tree), SM, true, Ignored, Mod, CheckTypes);
	  type_dependent -> annotate_maybe_error(cerl:case_arg(Tree), SM, Force, Ignored, Mod, CheckTypes);
	  false -> annotate_maybe_error(cerl:case_arg(Tree), SM, Force, Ignored, Mod, CheckTypes)
	end,
      NewMaybe_Error = maybe_error_or([get_maybe_error(Arg), ClausesError]),
      {cerl:update_c_case(update_ann(Tree, NewMaybe_Error), Arg, Clauses), C1 or C2, sets:union([Found1, Found2]), IgnoreFound1 or IgnoreFound2};
    clause ->
      {Pats, C1, Found1, SM1} = annotate_maybe_error_pattern_all(cerl:clause_pats(Tree), SM, Force),
      IgnoreFound1 = false,
      {Guard, C2, Found2, IgnoreFound2} = annotate_maybe_error(cerl:clause_guard(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      {Body, C3, Found3, IgnoreFound3} = annotate_maybe_error(cerl:clause_body(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      NewIgnoreFound = IgnoreFound1 or IgnoreFound2 or IgnoreFound3,
      NewMaybe_Error = maybe_error_or([get_maybe_error(Body), get_all_maybe_error(Pats), get_maybe_error(Guard)]),
      {cerl:update_c_clause(update_ann(Tree, NewMaybe_Error), Pats, Guard, Body), C1 or C2 or C3, sets:union([Found1, Found2, Found3]), NewIgnoreFound};
    cons ->
      {Hd, C1, Found1, IgnoreFound1} = annotate_maybe_error(cerl:cons_hd(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Tl, C2, Found2, IgnoreFound2} = annotate_maybe_error(cerl:cons_tl(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewIgnoreFound = IgnoreFound1 or IgnoreFound2,
      NewMaybe_Error = maybe_error_or([get_maybe_error(Hd), get_maybe_error(Tl)]),
      {cerl:update_c_cons_skel(update_ann(Tree, NewMaybe_Error), Hd, Tl), C1 or C2, sets:union([Found1, Found2]), NewIgnoreFound};
    'fun' ->
      Flags = make_fun_flags(cerl:fun_vars(Tree)),
      SM1 = put_vars(cerl:fun_vars(Tree), Flags, SM),
      {Vars, C1, Found1, IgnoreFound1} = annotate_maybe_error_all(cerl:fun_vars(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      {Body, C2, Found2, IgnoreFound2} = annotate_maybe_error(cerl:fun_body(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = maybe_error_or([get_maybe_error(Body), get_all_maybe_error(Vars)]),
      {cerl:update_c_fun(update_ann(Tree, NewMaybe_Error), Vars, Body), C1 or C2, sets:union([Found1, Found2]), IgnoreFound1 or IgnoreFound2};
    'let' ->
      {Arg, C2, Found1, IgnoreFound1} = annotate_maybe_error(cerl:let_arg(Tree), SM, Force, Ignored, Mod, CheckTypes),
      SM1 = put_vars(cerl:let_vars(Tree), get_arg_maybe_errors(Arg), SM),
      {Vars, C1, Found2, IgnoreFound2} = annotate_maybe_error_all(cerl:let_vars(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      {Body, C3, Found3, IgnoreFound3} = annotate_maybe_error(cerl:let_body(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      Tree1 =
	case vars_in_set(cerl:let_vars(Tree), Found3) of
	  true ->
	    add_force_constraint_logging(Tree);
	  false ->
	    Tree
	end,  
      NewMaybe_Error = maybe_error_or([get_all_maybe_error(Vars), get_maybe_error(Arg), get_maybe_error(Body)]),
      NewIgnoreFound = IgnoreFound1 or IgnoreFound2 or IgnoreFound3,
      {cerl:update_c_let(update_ann(Tree1, NewMaybe_Error), Vars, Arg, Body), C1 or C2 or C3, sets:union([Found1, Found2, Found3]), NewIgnoreFound};
    letrec ->
      {Names, Funsb} = lists:unzip(cerl:letrec_defs(Tree)),
      {Funs, C1, Found1, IgnoreFound1} = annotate_maybe_error_all(Funsb, SM, Force, Ignored, Mod, CheckTypes),
      SM1 = put_vars(Names, [{get_maybe_error_pessimistic(A), letvar} || A <- Funs], SM),
      {Body, C2, Found2, IgnoreFound2} = annotate_maybe_error(cerl:letrec_body(Tree), SM1, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = get_maybe_error(Body),
      {cerl:update_c_letrec(update_ann(Tree, NewMaybe_Error), lists:zip(Names, Funs), Body), C1 or C2, sets:union([Found1, Found2]), IgnoreFound1 or IgnoreFound2};
    literal ->
      {update_ann(Tree, false), true == CurMaybe_Error, sets:new(), false};
    primop ->
      {update_ann(Tree, true), false == CurMaybe_Error, sets:new(), false};
    'receive' -> throw("Error annotation not supporting receive at the moment");
    seq ->
      {Arg, C1, Found1, IgnoreFound1} = annotate_maybe_error(cerl:seq_arg(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Body, C2, Found2, IgnoreFound2} = annotate_maybe_error(cerl:seq_body(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewIgnoreFound = IgnoreFound1 or IgnoreFound2,
      NewMaybe_Error = maybe_error_or([get_maybe_error(Arg), get_maybe_error(Body)]),
      {cerl:update_c_seq(update_ann(Tree, NewMaybe_Error), Arg, Body), C1 or C2, sets:union([Found1, Found2]), NewIgnoreFound};
    'try' ->
      {Arg, C1, Found1, IgnoreFound1} = annotate_maybe_error(cerl:try_arg(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Vars, C2, Found2, IgnoreFound2} = annotate_maybe_error_all(cerl:try_vars(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Body, C3, Found3, IgnoreFound3} = annotate_maybe_error(cerl:try_body(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Evars, C4, Found4, IgnoreFound4} = annotate_maybe_error_all(cerl:try_evars(Tree), SM, Force, Ignored, Mod, CheckTypes),
      {Handler, C5, Found5, IgnoreFound5} = annotate_maybe_error(cerl:try_handler(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewIgnoreFound = IgnoreFound1 or IgnoreFound2 or IgnoreFound3 or IgnoreFound4 or IgnoreFound5,
      NewMaybe_Error = get_maybe_error(Arg),
      {cerl:update_c_try(update_ann(Tree, NewMaybe_Error), Arg, Vars, Body, Evars, Handler), C1 or C2 or C3 or C4 or C5, sets:union([Found1, Found2, Found3, Found4, Found5]), NewIgnoreFound};
%    'catch' ->
    tuple ->
      {Es, C, Found, IgnoreFound} = annotate_maybe_error_all(cerl:tuple_es(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = get_all_maybe_error(Es),
      {cerl:update_c_tuple(update_ann(Tree, NewMaybe_Error), Es), C, Found, IgnoreFound};
    values ->
      {Es, C, Found, IgnoreFound} = annotate_maybe_error_all(cerl:values_es(Tree), SM, Force, Ignored, Mod, CheckTypes),
      NewMaybe_Error = get_all_maybe_error(Es),
      {cerl:update_c_values(update_ann(Tree, NewMaybe_Error), Es), C, Found, IgnoreFound};
    var ->
      Found =
	case Force of
	  true -> sets:add_element(cerl:var_name(Tree), sets:new());
	  false -> sets:new()
	end,
      case dict:find(cerl:var_name(Tree), SM) of
	{ok, {Value, _}} ->
	  {update_ann(Tree, Value), Value =/= CurMaybe_Error, Found, false};
	error ->
	  {update_ann(Tree, true), true =/= CurMaybe_Error, Found, false}
      end;
    _ ->
      {update_ann(Tree, true), true =/= CurMaybe_Error, sets:new(), false}
  end.

annotate_maybe_error_pattern(Tree, SM, Force) ->
  CurMaybe_Error = get_maybe_error(Tree),
  case cerl:type(Tree) of
    literal ->
      {update_ann(Tree, false), true == CurMaybe_Error, sets:new(), SM};
    var ->
      Found =
	case Force of
	  true -> sets:add_element(cerl:var_name(Tree), sets:new());
	  false -> sets:new()
	end,
      case dict:find(cerl:var_name(Tree), SM) of
	{ok, {Value, _}} ->
	  {update_ann(Tree, Value), Value =/= CurMaybe_Error, Found, SM};
	error ->
	  {update_ann(Tree, false), false =/= CurMaybe_Error, Found, put_vars([Tree], [{type_dependent, 'var'}], SM)}
      end;
    cons ->
      {Hd, C1, Found1, SM1} = annotate_maybe_error_pattern(cerl:cons_hd(Tree), SM, Force),
      {Tl, C2, Found2, SM2} = annotate_maybe_error_pattern(cerl:cons_tl(Tree), SM1, Force),
      NewMaybe_Error = maybe_error_or([get_maybe_error(Hd), get_maybe_error(Tl)]),
      {cerl:update_c_cons_skel(update_ann(Tree, NewMaybe_Error), Hd, Tl), C1 or C2, sets:union([Found1, Found2]), SM2};
    tuple ->
      {Es, C, Found, SM1} = annotate_maybe_error_pattern_all(cerl:tuple_es(Tree), SM, Force),
      NewMaybe_Error = get_all_maybe_error(Es),
      {cerl:update_c_tuple(update_ann(Tree, NewMaybe_Error), Es), C, Found, SM1}
  end.
  
-spec get_arg_maybe_errors(cerl:cerl()) -> [{maybe_error(), atom()}].
get_arg_maybe_errors(Arg) ->
  [{get_maybe_error_pessimistic(Arg), letvar}].

annotate_maybe_error_all(Trees, SM, Force, Ignored, Mod, CheckTypes) ->
  X = [annotate_maybe_error(T, SM, Force, Ignored, Mod, CheckTypes) || T <- Trees],
  MyOr = fun(E) -> fun(A, B) -> B or element(E, A) end end,
  {[element(1, Y) || Y <- X], lists:foldl(MyOr(2), false, X), sets:union([element(3, Z) || Z <- X]), lists:foldl(MyOr(4), false, X)}.

annotate_maybe_error_pattern_all(Trees, SM, Force) ->
  annotate_maybe_error_pattern_all(Trees, SM, Force, [], false, sets:new()).

annotate_maybe_error_pattern_all([], SM, _, AccTrees, AccC, AccFound) -> {lists:reverse(AccTrees), AccC, AccFound, SM};
annotate_maybe_error_pattern_all([Tree|Trees], SM, Force, AccTrees, AccC, AccFound) ->
  {NewTree, C, Found, SM1} = annotate_maybe_error_pattern(Tree, SM, Force),
  annotate_maybe_error_pattern_all(Trees, SM1, Force, [NewTree|AccTrees], C or AccC, sets:union([AccFound, Found])).

-spec get_maybe_error(cerl:cerl()) -> maybe_error().
get_maybe_error(Tree) ->
  Anno = cerl:get_ann(Tree),
  get_maybe_error_anno(Anno).

-spec get_maybe_error_anno([any()]) -> maybe_error().
get_maybe_error_anno([]) -> false;
get_maybe_error_anno([{maybe_error, V}|_]) -> V;
get_maybe_error_anno([_|Tl]) -> get_maybe_error_anno(Tl).

-spec get_maybe_error_bin(cerl:cerl(), boolean()) -> boolean().
get_maybe_error_bin(Tree, DT) ->
  Anno = cerl:get_ann(Tree),
  get_maybe_error_bin_anno(Anno, DT).

-spec get_maybe_error_bin_anno([any()], boolean()) -> boolean().
get_maybe_error_bin_anno([], _DT) -> true;
get_maybe_error_bin_anno([{maybe_error, V}|_], DT) -> 
  case V of
    type_dependent -> DT;
    V1 -> V1
  end;
get_maybe_error_bin_anno([_|Tl], DT) -> get_maybe_error_bin_anno(Tl, DT).

get_maybe_error_pessimistic(Tree) ->
  get_maybe_error_pessimistic_anno(cerl:get_ann(Tree)).

get_maybe_error_pessimistic_anno([]) -> true;
get_maybe_error_pessimistic_anno([{maybe_error, V}|_]) -> V; 
get_maybe_error_pessimistic_anno([_|Tl]) -> get_maybe_error_pessimistic_anno(Tl).
  
-spec get_all_maybe_error([cerl:cerl()]) -> maybe_error().
get_all_maybe_error(Trees) ->
  maybe_error_or([get_maybe_error(T) || T <- Trees, not cuter_spec_checker:get_type_dependent_unreachable(T)]).

vars_in_set([], _) -> false;
vars_in_set([Hd|Tl], Set) ->
  case sets:is_element(cerl:var_name(Hd), Set) of
    true ->
      true;
    false ->
      vars_in_set(Tl, Set)
  end.

-spec get_force_constraint_logging([any()]) -> boolean().
get_force_constraint_logging([]) -> false;
get_force_constraint_logging([Hd|Tl]) ->
  case Hd of
    {force_constraint_logging, Value} ->
      Value;
    _ ->
      get_force_constraint_logging(Tl)
  end.

-spec get_distrust_type_dependent([any()]) -> boolean().
get_distrust_type_dependent([]) -> false;
get_distrust_type_dependent([Hd|Tl]) ->
  case Hd of
    {distrust_type_dependent, Value} ->
      Value;
    _ ->
      get_distrust_type_dependent(Tl)
  end.

-spec maybe_error_or([maybe_error()]) -> maybe_error().
maybe_error_or(E) ->
  lists:foldl(
    fun(A, B) ->
	case A of
	  true -> true;
	  false -> B;
	  type_dependent ->
	    case B of
	      true -> true;
	      _ -> type_dependent
	    end
	end
    end,
    false,
    E
   ).

unreachable_clauses(Clauses) ->
  lists:foldl(fun(Clause, Acc) -> Acc orelse cuter_spec_checker:get_type_dependent_unreachable(Clause) end, false, Clauses).

make_fun_flags(Vars) ->
  Fn = fun(Var) ->
	   case cuter_spec_checker:get_cerl_type(Var) of
	     notype -> {false, var};
	     T ->
	       case erl_types:t_is_fun(T) of
		 true -> {type_dependent, var};
		 false -> {false, var}
	       end
	   end
       end,
  lists:map(Fn, Vars).

%% ================================================================================
%% The preprocess function:
%% Takes an entry point {M, F, A}, calculates the callgraph with this entrypoint as
%% its root, merges the nodes belonging to a cycle until the callgraph is a DAG and
%% then annotates it from the leaves to the root, in a DFS order
%% ================================================================================

-spec preprocess(mfa(), dict:dict(), boolean()) -> dict:dict().
preprocess(EntryPoint, KFunctionASTS, CheckTypes) ->
  FunctionASTS = 
    dict:map(
      fun(_, Value) ->
	  cuter_cerl:kfun_code(Value)
      end,
      KFunctionASTS
     ),
  {CallGraph, _Funs, NewEntryPoint} = cuter_graphs:calculate_dag_callgraph(EntryPoint),
  AnnotatedASTS = annotate_callgraph(NewEntryPoint, FunctionASTS, CallGraph, CheckTypes),
  dict:map(
    fun(Key, Value) ->
	cuter_cerl:kfun_update_code(Value, dict:fetch(Key, AnnotatedASTS))
    end,
    KFunctionASTS
   ).

-spec preprocess(mfa(), dict:dict(), dict:dict(), boolean()) -> dict:dict().
preprocess(EntryPoint, KFunctionASTS, MfasToSpecs, CheckTypes) ->
  FunctionASTS = 
    dict:map(
      fun(_, Value) ->
	  cuter_cerl:kfun_code(Value)
      end,
      KFunctionASTS
     ),
  {CallGraph, Funs, NewEntryPoint} = cuter_graphs:calculate_dag_callgraph(EntryPoint),
  TypedASTS = cuter_spec_checker:annotate_types(FunctionASTS, MfasToSpecs, Funs),
  AnnotatedASTS = annotate_callgraph(NewEntryPoint, TypedASTS, CallGraph, CheckTypes),
  dict:map(
    fun(Key, Value) ->
	cuter_cerl:kfun_update_code(Value, dict:fetch(Key, AnnotatedASTS))
    end,
    KFunctionASTS
   ).
