-module(cuter_spec_checker).
-export([get_cerl_type/1, get_type_dependent_unreachable/1, annotate_types/3]).

%% =========================
%% multi function annotation
%% =========================

-spec annotate_types(dict:dict(), dict:dict(), sets:set()) -> dict:dict().
annotate_types(FunctionASTS, Sigs, FSet) ->
  TSM = 
    lists:foldl(
      fun ({MFA, Sig}, T) ->
	  dict:store(MFA, Sig, T)
      end,
      cuter_type_dependent_functions:original_tsm(),
      dict:to_list(Sigs)
     ),
  NoSpec = find_nospec(FSet, Sigs),
  OpenSet = make_open_set(FSet, Sigs),
  annotate_types_helper(FunctionASTS, TSM, OpenSet, NoSpec).

annotate_types_helper(FunctionASTS, TSM, OpenSet, NoSpec) ->
  case length(OpenSet) of
    0 -> FunctionASTS;
    _ ->
      {FASTS1, TSM1, OpenSet1} = annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec),
      annotate_types_helper(FASTS1, TSM1, OpenSet1, NoSpec)
  end.

annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec) ->
  annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec, []).

annotate_types_helper_pass(FunctionASTS, TSM, [], _NoSpec, OpenSet1) -> {FunctionASTS, TSM, lists:reverse(OpenSet1)};
annotate_types_helper_pass(FunctionASTS, TSM, [Mfa|Mfas], NoSpec, OpenSet1) -> 
  AST = dict:fetch(Mfa, FunctionASTS),
  Spec = dict:fetch(Mfa, TSM),
  {NewAST, D, C} = pass_down_fun_types(Mfa, AST, Spec, TSM, NoSpec),
  {TSM1, OpenSet2} = update_from_detected(D, TSM, OpenSet1),
  case C or (length(D) > 0) of
    true ->
      OpenSet3 = [Mfa|OpenSet2];
    false ->
      OpenSet3 = OpenSet2
  end,
  case sets:is_element(Mfa, NoSpec) of
    true ->
      T = get_cerl_type(NewAST),
      case erl_types:is_erl_type(T) of
	true ->
	  [S] = dict:fetch(Mfa, TSM1),
	  NewS = erl_types:t_fun(erl_types:t_fun_args(S), T),
	  TSM2 = dict:store(Mfa, [NewS], TSM1);
	false -> TSM2 = TSM1
      end;
    false ->
      TSM2 = TSM1
  end,
  NewASTS = dict:store(Mfa, NewAST, FunctionASTS),
  annotate_types_helper_pass(NewASTS, TSM2, Mfas, NoSpec, OpenSet3).

update_from_detected([], TSM, OpenSet) -> {TSM, OpenSet};
update_from_detected([{Mfa, Spec}|Rest], TSM, OpenSet) ->
  OpenSet1 = [Mfa|OpenSet],
  case dict:find(Mfa, TSM) of
    {ok, [Cur]} ->
      TSM1 = dict:store(Mfa, [erl_types:t_sup(Cur, Spec)], TSM);
    error ->
      TSM1 = dict:store(Mfa, [Spec], TSM)
  end,
  update_from_detected(Rest, TSM1, OpenSet1).

find_nospec(FSet, Sigs) ->
  Fn = fun(F) -> not dict:is_key(F, Sigs) end,
  sets:filter(Fn, FSet).

make_open_set(FSet, Sigs) ->
  Fn = fun(F) ->
	   case dict:is_key(F, Sigs) of
	     true -> length(dict:fetch(F, Sigs)) =:= 1;
	     false -> false
	   end
       end,
  sets:to_list(sets:filter(Fn, FSet)).

%% ==========================
%% single function annotation
%% ==========================

get_type([]) -> notype;
get_type([Hd|Tl]) ->
  case Hd of
    {node_type, Value} ->
      Value;
    _ ->
      get_type(Tl)
  end.

-spec get_cerl_type(cerl:cerl()) -> erl_types:erl_type() | notype.
get_cerl_type(T) -> get_type(cerl:get_ann(T)).

update_type(Tree, Type) ->
  Anno = cerl:get_ann(Tree),
  cerl:set_ann(Tree, update_type(Anno, Type, [], false)).

update_type([], Type, Acc, false) -> [{node_type, Type}|Acc];
update_type([], _, Acc, true) -> Acc;
update_type([{node_type, _}|T], Type, Acc, _) -> update_type(T, Type, [{node_type, Type}|Acc], true);
update_type([H|T], Type, Acc, Found) -> update_type(T, Type, [H|Acc], Found).

mark_as_unreachable(Clause) ->
  Anno = cerl:get_ann(Clause),
  case cuter_graphs:list_contains(type_dependent_unreachable, Anno) of
    false ->
      cerl:add_ann([type_dependent_unreachable], Clause);
    true ->
      Clause
  end.

mark_as_reachable(Clause) ->
  Anno = [T || T <- cerl:get_ann(Clause), T =/= type_dependent_unreachable],
  cerl:set_ann(Clause, Anno).

has_type(Tree) ->
  Anno = cerl:get_ann(Tree),
  lists:foldl(
    fun erlang:'or'/2,
    false,
    lists:map(
      fun(A) ->
	  case A of
	    {node_type, T} when T =/= notype -> true;
	    _ -> false
	  end
      end,
      Anno
     )
   ).

arg_types(Args) ->
  lists:map(fun get_cerl_type/1, Args).

let_arg_types(Arg) ->
  case cerl:type(Arg) of
    values ->
      arg_types(cerl:values_es(Arg));
    _ -> [get_cerl_type(Arg)]
  end.

put_vars(Vars, Types, TSM) ->
  F =
    fun({Var, Type}, B) ->
	case Type of
	  notype -> B;
	  [notype] -> B;
	  _ -> dict:store(cerl:var_name(Var), Type, B)
	end
    end,
  lists:foldl(F, TSM, lists:zip(Vars, Types)).

%% =====================
%% helper type functions
%% =====================

t_from_pattern(Tree, TSM, TSM2) ->
  case cerl:type(Tree) of
    literal ->
      erl_types:t_from_term(element(3, Tree));
    var ->
      case dict:find(cerl:var_name(Tree), TSM2) of
	{ok, Type} ->
	  Type;
	error ->
	  case dict:find(cerl:var_name(Tree), TSM) of
	    {ok, _} -> erl_types:t_none();
	    error ->
	      erl_types:t_any()
	  end
      end;
    cons ->
      Hd = t_from_pattern(cerl:cons_hd(Tree), TSM, TSM2),
      Tl = t_from_pattern(cerl:cons_tl(Tree), TSM, TSM2),
      case erl_types:t_is_nil(Tl) of
	true -> erl_types:t_none();
	false ->
	  case erl_types:t_is_none(Tl) of
	    true -> erl_types:t_none();
	    false -> erl_types:t_cons(Hd, Tl)
	  end
      end;
    tuple ->
      Es = lists:map(fun(E) -> t_from_pattern(E, TSM, TSM2) end, cerl:tuple_es(Tree)),
      erl_types:t_tuple(Es);
    alias ->
      Pat = cerl:alias_pat(Tree),
      t_from_pattern(Pat, TSM, TSM2);
    _ -> erl_types:t_none()
  end.

application_type(Spec, ArgTypes) when not is_list(Spec) ->
  application_type([Spec], ArgTypes);
application_type([], _) -> error;  
application_type([Spec|Specs], ArgTypes) ->
  SpecArgs = erl_types:t_fun_args(Spec),
  case lists:foldl(
	 fun erlang:'and'/2,
	 true,
	 lists:zipwith(
	   fun erl_types:t_is_subtype/2,
	   lists:map(
	     fun(A) ->
		 case A of
		   notype -> erl_types:t_any();
		   B -> B
		 end
	     end,
	     ArgTypes),
	   SpecArgs)) of
    true ->
      {ok, erl_types:t_fun_range(Spec)};
    false ->
      application_type(Specs, ArgTypes)
  end.

t_union(Types) ->
  t_union(Types, erl_types:t_none()).

t_union([], T) -> T;
t_union([Type|Types], T) -> t_union(Types, erl_types:t_sup(Type, T)).

unify_pattern(Tree, TSM, Type) ->
  case cerl:type(Tree) of
    literal ->
      {ok, TSM};
    var ->
      case dict:find(cerl:var_name(Tree), TSM) of
	{ok, VarType} ->
	  try erl_types:t_unify(VarType, Type) of
	    _ -> {ok, TSM}
	  catch
	    _ -> {error, mismatch}
	  end;
	error ->
	  {ok, dict:store(cerl:var_name(Tree), Type, TSM)}
      end;
    cons ->
      case erl_types:t_is_list(Type) of
	true ->
	  NewType = erl_types:t_nonempty_list(erl_types:t_list_elements(Type)),
	  Hdt = unify_pattern(cerl:cons_hd(Tree), TSM, erl_types:t_cons_hd(NewType)),
	  case Hdt of
	    {ok, TSM1} ->
	      Tlt = unify_pattern(cerl:cons_tl(Tree), TSM1, erl_types:t_cons_tl(NewType)),
	      case Tlt of
		{ok, TSM2} -> {ok, TSM2};
		_ ->{error, mismatch}
	      end;
	    _  ->
	      {error, mismatch}
	  end;
	false ->
	  {error, mismatch}
      end;
    tuple ->
      case erl_types:t_is_tuple(Type) of
	true ->
	  case length(cerl:tuple_es(Tree)) == erl_types:t_tuple_size(Type) of
	    true -> 
	      lists:foldl(
		fun({E, Et}, V) ->
		    case V of
		      {ok, V1} ->
			unify_pattern(E, V1, Et);
		      {error, _} ->
			{error, mismatch}
		    end
		end,
		{ok, TSM},
		lists:zip(cerl:tuple_es(Tree), erl_types:t_tuple_args(Type))
	       );
	    false -> {error, mismatch}
	  end;
	false -> {error, mismatch}
      end;
    _ ->
      {ok, TSM}
  end.


%% ==================
%% passing down types
%% ==================

pass_down_fun_types({M, _F, _A}, AST, Spec, TSM, NoSpec) ->
  pass_down_types_helper(AST, Spec, TSM, M, NoSpec).

pass_down_types_helper(Fun, Spec, TSM, Mod, NoSpec) ->
  TSM2 = put_vars(cerl:fun_vars(Fun), erl_types:t_fun_args(hd(Spec)), TSM),
  {Body, D, C} = pass_down_types(cerl:fun_body(Fun), TSM2, Mod, notype, NoSpec),
  {cerl:update_c_fun(Fun, cerl:fun_vars(Fun), Body), D, C}.

pass_down_types(Tree, TSM, Mod, ArgType, NoSpec) ->
  CurType = get_cerl_type(Tree),
  case cerl:type(Tree) of
    alias ->
      {Pat, D1, C1} = pass_down_types(cerl:alias_pat(Tree), TSM, Mod, ArgType, NoSpec),
      Var = cerl:alias_var(Tree),
      T = get_cerl_type(Pat),
      Var1 = update_type(Var, T),
      Change = C1 or (CurType =/= T),
      Tree1 = update_type(Tree, T),
      {cerl:update_c_alias(Tree1, Var1, Pat), D1, Change};
    'apply' ->
      {Args, D1, C1} = pass_down_types_all(cerl:apply_args(Tree), TSM, Mod, ArgType, NoSpec),
      Op = cerl:apply_op(Tree),
      {Tree1, D2, C2} =
	case lists:all(fun has_type/1, Args) of
	  true ->
	    case cerl:type(Op) of
	      var ->
		OpN = case cerl:var_name(Op) of {F, A} -> {Mod, F, A}; Name -> Name end,
		case dict:find(OpN, TSM) of
		  {ok, Specs} ->
		    case application_type(Specs, arg_types(Args)) of
		      {ok, Type} ->
			{update_type(Tree, Type), D1, false};
		      error ->
			case sets:is_element(OpN, NoSpec) of
			  true ->
			    NewSpec = rewrite_spec(arg_types(Args), Specs),
			    {Tree, [{OpN, NewSpec} | D1], true};
			  false ->
			    {Tree, D1, false}
			end
		    end;
		  error ->
		    case sets:is_element(OpN, NoSpec) of
		      true ->
			{Tree, [{OpN, erl_types:t_fun(arg_types(Args), erl_types:t_any())} | D1], true};
		      false ->
			{Tree, D1, false}
		    end
		end;
	      _ ->
		error("unhandled op")
	    end;
	  _ -> {Tree, D1, false}
	end,
      Change = C1 or C2 or (CurType =/= get_cerl_type(Tree1)),
      {cerl:update_c_apply(Tree1, Op, Args), D2, Change};
    %%binary -> meta
    %%bitstr -> meta
    call ->
      {Args, D1, C1} = pass_down_types_all(cerl:call_args(Tree), TSM, Mod, ArgType, NoSpec),
      ModName = cerl:call_module(Tree),
      Name = cerl:call_name(Tree),
      Arity = length(cerl:call_args(Tree)),
      {Tree1, D2, C2} =
	case lists:all(fun has_type/1, Args) of
	  true ->
	    case cerl:is_literal(ModName) andalso cerl:is_literal(Name) of
	      true ->
		OpN = {element(3, ModName), element(3, Name), Arity},
		case dict:find(OpN, TSM) of
		  {ok, Specs} ->
		    case application_type(Specs, arg_types(Args)) of
		      {ok, Type} ->
			{update_type(Tree, Type), D1, false};
		      _ ->
			case sets:is_element(OpN, NoSpec) of
			  true ->
			    NewSpec = rewrite_spec(arg_types(Args), Specs),
			    {Tree, [{OpN, NewSpec} | D1], true};
			  false -> {Tree, D1, false}
			end
		    end;
		  error ->
		    case sets:is_element(OpN, NoSpec) of
		      true ->
			{Tree, [{OpN, erl_types:t_fun(arg_types(Args), erl_types:t_any())} | D1], true};
		      false ->
			{Tree, D1, false}
		    end
		end;
	      _ -> throw("Unsupported call")
	    end;
	  _ -> {Tree, D1, false}
	end,
      Change = C1 or C2 or (CurType =/= get_cerl_type(Tree1)),
      {cerl:update_c_call(Tree1, ModName, Name, Args), D2, Change};
    'case' ->
      {Arg, D1, C1} = pass_down_types(cerl:case_arg(Tree), TSM, Mod, ArgType, NoSpec),
      {Clauses1, D2, C2} = pass_down_types_all(cerl:case_clauses(Tree), TSM, Mod, get_cerl_type(Arg), NoSpec),
      Clauses = mark_unreachable_clauses(Clauses1, get_cerl_type(Arg), TSM, Arg),
      Clauses2 = [Clause || Clause <- Clauses, not get_type_dependent_unreachable(Clause)],
      Type = 
	case lists:all(fun has_type/1, Clauses2) of
	  true ->
	    T = arg_types(Clauses2),
	    case listcontains(notype, T) of
	      true -> notype;
	      false -> t_union(T)
	    end;
	  false ->
	    notype
	end,
      Change = C1 or C2 or (CurType =/= Type),
      {cerl:update_c_case(update_type(Tree, Type), Arg, Clauses), lists:append(D1, D2), Change};
    clause ->
      Fn = fun({Pat, AType}, V) ->
	       case V of
		 {ok, V1} ->
		   unify_pattern(Pat, V1, AType);
		 {error, mismatch} -> {error, mismatch}
	       end
	   end,
      case length(cerl:clause_pats(Tree)) > 1 of
	true ->
	  case erl_types:t_is_tuple(ArgType) of
	    true ->
	      ATypes = erl_types:t_tuple_args(ArgType),
	      case length(ATypes) =:= length(cerl:clause_pats(Tree)) of
		true ->
		  ArgTypes = ATypes;
		false ->
		  ArgTypes = [notype || _ <- cerl:clause_pats(Tree)]
	      end;
	    false -> ArgTypes = [notype || _ <- cerl:clause_pats(Tree)]
	  end;
	false -> ArgTypes = [ArgType]
      end,
      case length(ArgTypes) =/= length(cerl:clause_pats(Tree)) of
	true -> 
	  TSMt = {error, arglen};
	false ->
	  TSMt = lists:foldl(Fn, {ok, TSM}, lists:zip(cerl:clause_pats(Tree), ArgTypes))
      end,
      case TSMt of
	{ok, TSMU} ->
	  TSM1 = TSMU;
	{error, _} ->
	  TSM1 = TSM
      end,
      {Pats, D1, C1} = pass_down_types_all(cerl:clause_pats(Tree), TSM1, Mod, ArgType, NoSpec),
      {Guard, D2, C2} = pass_down_types(cerl:clause_guard(Tree), TSM1, Mod, ArgType, NoSpec),
      {Body, D3, C3} = pass_down_types(cerl:clause_body(Tree), TSM1, Mod, ArgType, NoSpec),
      Change = C1 or C2 or C3 or (CurType =/= get_cerl_type(Body)),
      D = lists:append([D1, D2, D3]),
      {cerl:update_c_clause(update_type(Tree, get_cerl_type(Body)), Pats, Guard, Body), D, Change};
    cons ->
      {Hd, D1, C1} = pass_down_types(cerl:cons_hd(Tree), TSM, Mod, ArgType, NoSpec),
      {Tl, D2, C2} = pass_down_types(cerl:cons_tl(Tree), TSM, Mod, ArgType, NoSpec),
      Tree1 = 
	case {get_cerl_type(Hd), get_cerl_type(Tl)} of
	  {X, Y} when X =:= notype orelse Y =:= notype -> update_type(Tree, notype);
	  _ -> update_type(Tree, erl_types:t_cons(get_cerl_type(Hd), get_cerl_type(Tl)))
	end,
      Change = C1 or C2 or (CurType =/= get_cerl_type(Tree1)),
      D = lists:append([D1, D2]),
      {cerl:update_c_cons(Tree1, Hd, Tl), D, Change};
    tuple ->
      {Es, D, C} = pass_down_types_all(cerl:tuple_es(Tree), TSM, Mod, ArgType, NoSpec),
      Tree1 =
	case lists:foldl(fun(X, Y) -> Y orelse (get_cerl_type(X) =:= notype) end, false, Es) of
	  true ->
	    update_type(Tree, notype);
	  false -> update_type(Tree, erl_types:t_tuple(lists:map(fun get_cerl_type/1, Es)))
	end,
      Change = C or (CurType =/= get_cerl_type(Tree1)),
      {cerl:update_c_tuple(Tree1, Es), D, Change};
    'fun' ->
      TSM1 = put_vars(cerl:fun_vars(Tree), [erl_types:t_any() || _ <- cerl:fun_vars(Tree)], TSM),
      {Vars, _D1, _C1} = pass_down_types_all(cerl:fun_vars(Tree), TSM1, Mod, ArgType, NoSpec),
      {Body, D1, C1} = pass_down_types(cerl:fun_body(Tree), TSM1, Mod, ArgType, NoSpec),
      Tree1 =
	case has_type(Body) of
	  true ->
	    case get_cerl_type(Body) of
	      notype -> update_type(Tree, notype);
	      _ ->
		Type = erl_types:t_fun([erl_types:t_any() || _ <- cerl:fun_vars(Tree)], get_cerl_type(Body)),
		update_type(Tree, Type)
	    end;
	  _ -> update_type(Tree, notype)
	end,
      Change = C1 or (CurType =/= get_cerl_type(Tree1)),
      {cerl:update_c_fun(Tree1, Vars, Body), D1, Change};
    'let' ->
      {Arg, D1, C1} = pass_down_types(cerl:let_arg(Tree), TSM, Mod, ArgType, NoSpec),
      TSM1 = put_vars(cerl:let_vars(Tree), let_arg_types(Arg), TSM),
      {Vars, D2, C2} = pass_down_types_all(cerl:let_vars(Tree), TSM1, Mod, ArgType, NoSpec),
      {Body, D3, C3} = pass_down_types(cerl:let_body(Tree), TSM1, Mod, ArgType, NoSpec),
      Tree1 =
	case has_type(Body) of
	  true ->
	    update_type(Tree, get_cerl_type(Body));
	  false ->
	    update_type(Tree, notype)
	end,
      Change = C1 or C2 or C3 or (CurType =/= get_cerl_type(Tree1)),
      D = lists:append([D1, D2, D3]),
      {cerl:update_c_let(Tree1, Vars, Arg, Body), D, Change};
    letrec ->
      {Names, Funsb} = lists:unzip(cerl:letrec_defs(Tree)),
      {Funs, D1, C1} = pass_down_types_all(Funsb, TSM, Mod, ArgType, NoSpec),
      TSM1 = put_vars(Names, [[get_cerl_type(F)] || F <- Funs], TSM),
      {Body, D2, C2} = pass_down_types(cerl:letrec_body(Tree), TSM1, Mod, ArgType, NoSpec),
      Change = C1 or C2 or (CurType =/= get_cerl_type(Body)),
      D = lists:append(D1, D2),
      {cerl:update_c_letrec(update_type(Tree, get_cerl_type(Body)), lists:zip(Names, Funs), Body), D, Change};
    literal ->
      {update_type(Tree, erl_types:t_from_term(element(3, Tree))), [], false};
    seq ->
      {Arg, D1, C1} = pass_down_types(cerl:seq_arg(Tree), TSM, Mod, ArgType, NoSpec),
      {Body, D2, C2} = pass_down_types(cerl:seq_body(Tree), TSM, Mod, ArgType, NoSpec),
      Change = C1 or C2 or (CurType =/= get_cerl_type(Body)),
      D = lists:append(D1, D2),
      {cerl:update_c_seq(update_type(Tree, get_cerl_type(Body)), Arg, Body), D, Change};
    'try' ->
      {Arg, D1, C1} = pass_down_types(cerl:try_arg(Tree), TSM, Mod, ArgType, NoSpec),
      {Vars, D2, C2} = pass_down_types_all(cerl:try_vars(Tree), TSM, Mod, ArgType, NoSpec),
      {Body, D3, C3} = pass_down_types(cerl:try_body(Tree), TSM, Mod, ArgType, NoSpec),
      {Evars, D4, C4} = pass_down_types_all(cerl:try_evars(Tree), TSM, Mod, ArgType, NoSpec),
      {Handler, D5, C5} = pass_down_types(cerl:try_handler(Tree), TSM, Mod, ArgType, NoSpec),
      Change = C1 or C2 or C3 or C4 or C5 or (CurType =/= get_cerl_type(Body)),
      D = lists:append([D1, D2, D3, D4, D5]),
      {cerl:update_c_try(update_type(Tree, get_cerl_type(Body)), Arg, Vars, Body, Evars, Handler), D, Change};
    %%    'catch' ->
    primop ->
      {update_type(Tree, notype), [], false};
    values ->
      {Es, D1, C1} = pass_down_types_all(cerl:values_es(Tree), TSM, Mod, ArgType, NoSpec),
      case lists:all(fun has_type/1, Es) of
	true ->
	  {cerl:update_c_values(update_type(Tree, erl_types:t_tuple([get_cerl_type(T) || T <- Es])), Es), D1, C1};
	false ->
	  {cerl:update_c_values(update_type(Tree, notype), Es), D1, C1 or (CurType =/= notype)}
      end;
    var ->
      case dict:find(cerl:var_name(Tree), TSM) of
	{ok, Type} ->
	  {update_type(Tree, Type), [], false};
	_ -> {update_type(Tree, notype), [], false}
      end;
    _ -> 
      Tree
  end.

pass_down_types_all(Trees, TSM, Mod, ArgType, NoSpec) ->
  R = lists:map(fun(A) -> pass_down_types(A, TSM, Mod, ArgType, NoSpec) end, Trees),
  {NewTrees, AllDetected, Changes} = lists:unzip3(R),
  {NewTrees, lists:append(AllDetected), lists:foldl(fun erlang:'or'/2, false, Changes)}.

rewrite_spec(ArgTypes, [Spec]) ->
  erl_types:t_fun(ArgTypes, erl_types:t_fun_range(Spec)).

mark_unreachable_clauses(Clauses, ArgType, TSM, Arg) ->
  case cerl:type(Arg) =:= values of
    true ->
      ArgList = cerl:values_es(Arg);
    false ->
      ArgList = [Arg]
  end,
  case ArgType =:= notype of
    false ->
      mark_unreachable_clauses(Clauses, ArgType, TSM, ArgList, []);
    true -> Clauses
  end.

mark_unreachable_clauses([], _, _, _, NewClauses) -> lists:reverse(NewClauses);
mark_unreachable_clauses([Clause|Clauses], ArgType, TSM, Arg, NewClauses) ->
  Pats =  cerl:clause_pats(Clause),
  NewClause =
    case erl_types:t_is_none(ArgType) of
      true ->
	mark_as_unreachable(Clause);
      false ->
	mark_as_reachable(Clause)
    end,
  SafeSub = fun(A, B) ->
		try erl_types:t_subtract(A, B) of
		  T -> T
		catch
		  _:_ -> A
		end
	    end,
  {A, TSMorT} = update_tsm_from_guard(Clause, TSM, Arg),
  case A of
    {argtype, ArgName} ->
      PatTypes1 = lists:map(fun (X) -> t_from_pattern(X, TSM, dict:new()) end, Pats),
      PatTypes = [PatType || PatType <- PatTypes1, PatType =/= notype],
      case length(PatTypes) =:= length(Arg) of
	true ->
	  PatTypes2 = replace_guard_type(Arg, ArgName, PatTypes, TSMorT),
	  case length(PatTypes) > 1 of
	    true ->
	      PatTypes3 = erl_types:t_tuple(PatTypes2),
	      T = SafeSub(ArgType, PatTypes3);
	    false ->
	      PatTypes3 = hd(PatTypes2),
	      T = SafeSub(ArgType, PatTypes3)
	  end;
	false ->
	  T = ArgType
      end;
    tsm ->
      PatTypes1 = lists:map(fun (X) -> t_from_pattern(X, TSM, TSMorT) end, Pats),
      PatTypes = [PatType || PatType <- PatTypes1, PatType =/= notype],
      case length(PatTypes) =:= length(Arg) of
	true ->
	  case length(PatTypes) > 1 of
	    true ->
	      PatTypes3 = erl_types:t_tuple(PatTypes),
	      T = SafeSub(ArgType, PatTypes3);
	    false ->
	      PatTypes3 = hd(PatTypes),
	      T = SafeSub(ArgType, PatTypes3)
	  end;
	false ->
	  T = ArgType
      end;
    invalid ->
      T = ArgType
  end,
  mark_unreachable_clauses(Clauses, T, TSM, Arg, [NewClause|NewClauses]).

replace_guard_type([], _ArgName, [], _TSMorT) -> [];
replace_guard_type([Arg|Args], ArgName, [PatType|PatTypes], TSMorT) -> 
  case cerl:type(Arg) =:= var of
    true ->
      case cerl:var_name(Arg) =:= ArgName of
	true ->
	  [TSMorT|PatTypes];
	false ->
	  [PatType|replace_guard_type(Args, ArgName, PatTypes, TSMorT)]
      end;
    false ->
      [PatType|replace_guard_type(Args, ArgName, PatTypes, TSMorT)]
  end.
  
valid_guard(Clause, TSM, ArgList) ->
  Guard = cerl:clause_guard(Clause),
  case cerl:type(Guard) of
    literal when element(3, Guard) =:= true -> true;
    call ->      Args = cerl:call_args(Guard),
      case get_call_mfa(Guard) of
	{erlang, is_integer, 1} -> is_unknown_var(hd(Args), TSM, ArgList);
	{erlang, is_atom, 1} -> is_unknown_var(hd(Args), TSM, ArgList);
	{erlang, is_function, 1} -> is_unknown_var(hd(Args), TSM, ArgList);
	{erlang, is_function, 2} ->
	  C1 = is_unknown_var(hd(Args), TSM, ArgList),
	  C2 = cerl:type(lists:nth(2, Args)) =:= literal,
	  C1 or C2;
	_ -> false
      end;
    'try' -> 
      TryArg = cerl:try_arg(Guard),
      case cerl:type(TryArg) of 
	'let' ->
	  case length(cerl:let_vars(TryArg)) =:= 1 of
	    true ->
	      LetVar = hd(cerl:let_vars(TryArg)),
	      LetBody = cerl:let_body(TryArg),
	      LetArg = cerl:let_arg(TryArg),
	      case cerl:type(LetArg) of
		'call' ->
		  case get_call_mfa(LetArg) of
		    {erlang, is_function, 2} ->
		      case cerl:type(LetBody) of
			'call' ->
			  case is_right_call(LetBody, LetVar) of
			    true ->
			      is_unknown_var(hd(cerl:call_args(LetArg)), TSM, ArgList);
			    false -> false
			  end;
			_ -> false
			end;
		    _ -> false
		  end;
		_ -> false
	      end;
	    false -> false
	  end;
	_ -> false
      end;
    _ -> false
  end.

get_call_mfa(Guard) ->
  ModName = cerl:call_module(Guard),
  Name = cerl:call_name(Guard),
  Arity = length(cerl:call_args(Guard)),
  case cerl:type(ModName) =:= literal andalso cerl:type(Name) =:= literal of
    true -> {element(3, ModName), element(3, Name), Arity};
    false -> unmatched
  end.

is_unknown_var(X, TSM, ArgList) ->
  case cerl:type(X) of
    var ->
      ArgVarNames = [cerl:var_name(Var) || Var <- ArgList, cerl:type(Var) =:= var],
      case dict:find(cerl:var_name(X), TSM) of
	{ok, _} -> listcontains(cerl:var_name(X), ArgVarNames);
	error ->true
      end;
    _ -> false
  end.

is_right_call(Call, LetVar) ->
  case get_call_mfa(Call) =:= {erlang, '=:=', 2} of
    true ->
      [Arg1, Arg2] = cerl:call_args(Call),
      case cerl:type(Arg1) =:= var andalso cerl:type(Arg2) =:= literal of
	true -> cerl:var_name(LetVar) =:= cerl:var_name(Arg1) andalso element(3, Arg2) =:= true;
	false -> false
      end;
    false -> false
  end.

update_tsm_from_guard(Clause, TSM, ArgList) ->
  case valid_guard(Clause, TSM, ArgList) of
    true ->
      Guard = cerl:clause_guard(Clause),
      case cerl:type(Guard) of
	literal when element(3, Guard) =:= true -> {tsm, TSM};
	call ->
	  Args = cerl:call_args(Guard),
	  case get_call_mfa(Guard) of
	    {erlang, is_integer, 1} ->
	      update_tsm_from_guard_helper(Args, ArgList, erl_types:t_integer());
	    {erlang, is_atom, 1} -> 
	      update_tsm_from_guard_helper(Args, ArgList, erl_types:t_atom());
	    {erlang, is_function, 1} ->
	      update_tsm_from_guard_helper(Args, ArgList, erl_types:t_fun());
	    {erlang, is_function, 2}->
	      Arity = element(3, lists:nth(2, Args)),
	      update_tsm_from_guard_helper(Args, ArgList, erl_types:t_fun(Arity, erl_types:t_any()))
	  end;
	'try' ->
	  TryArg = cerl:try_arg(Guard),
	  LetArg = cerl:let_arg(TryArg),
	  Args = cerl:call_args(LetArg),
	  case get_call_mfa(LetArg) of
	    {erlang, is_function, 2} ->
	      Arity = element(3, lists:nth(2, Args)),
	      update_tsm_from_guard_helper(Args, ArgList, erl_types:t_fun(Arity, erl_types:t_any()))
	  end
      end;
    false ->
      {invalid, none}
  end.

update_tsm_from_guard_helper(Args, ArgList, Type) ->
  FunArgName = cerl:var_name(hd(Args)),
  ArgVarNames = [cerl:var_name(Var) || Var <- ArgList, cerl:type(Var) =:= var],
  case listcontains(FunArgName, ArgVarNames) of
    true -> {{argtype, FunArgName}, Type};
    _ -> {tsm, dict:store(FunArgName, Type, dict:new())}
  end.

get_ann_type_dependent_unreachable([]) -> false;
get_ann_type_dependent_unreachable([Hd|Tl]) ->
  case Hd of
    type_dependent_unreachable ->
      true;
    _ ->
      get_ann_type_dependent_unreachable(Tl)
  end.

-spec get_type_dependent_unreachable(cerl:cerl()) -> boolean().
get_type_dependent_unreachable(T) -> get_ann_type_dependent_unreachable(cerl:get_ann(T)).  

listcontains(_, []) -> false;
listcontains(X, [H|_]) when X =:= H -> true;
listcontains(X, [H|T]) when X =/= H -> listcontains(X, T).
