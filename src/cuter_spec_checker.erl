-module(cuter_spec_checker).
-export([get_cerl_type/1, get_type_dependent_unreachable/1, annotate_types/3]).

%% ==========
%% Used types
%% ==========

-type function_ast_dict() :: dict:dict(mfa(), cerl:cerl()).
-type function_sig_dict() :: dict:dict(mfa(), [erl_types:erl_type()]).
-type function_set() :: sets:set(mfa()).

%% =========================
%% Multi function annotation
%% =========================

%% Entry point for the type annotation of the function ASTs.
%% Arguments:
%% FunctionASTS: dictionary with Mfas as keys and ASTs as values.
%% Sigs: dictionary with Mfas as keys and lists of erl_types as values.
%% FSet: Set of all functions in the callgraph.
%% Returns:
%% The annotated function ASTs in a dictionary with their Mfas as keys.
-spec annotate_types(function_ast_dict(), function_sig_dict(), function_set()) -> function_ast_dict().
annotate_types(FunctionASTS, Sigs, FSet) ->
  %% Initialize the symbol table using the original_tsm defined in cuter_type_dependent_functions
  %% adding the signatures of the functions we want to annotate too.
  TSM = 
    lists:foldl(
      fun ({MFA, Sig}, T) ->
          dict:store(MFA, Sig, T)
      end,
      cuter_type_dependent_functions:original_tsm(),
      dict:to_list(Sigs)
     ),
  %% NoSpec will hold the funtions with no signatures.
  %% We need to know which of them do not originally have signatures
  %% to allow altering their signature when we find calls to them.
  NoSpec = find_nospec(FSet, Sigs),
  %% OpenSet will hold the Mfas of functions we want to traverse.
  %% It also contains a persistence dictionary for each function to avoid
  %% infinite loops in recursive letrec.
  OpenSet = make_open_set(FSet, Sigs),
  annotate_types_helper(FunctionASTS, TSM, OpenSet, NoSpec).

%% Performs the fix point computation over the functions in OpenSet.
%% While OpenSet has elements, the annotation algorithm is ran upon the
%% whole OpenSet.
annotate_types_helper(FunctionASTS, TSM, OpenSet, NoSpec) ->
  case length(OpenSet) of
    0 -> FunctionASTS;
    _ ->
      {FASTS1, TSM1, OpenSet1} = annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec),
      annotate_types_helper(FASTS1, TSM1, OpenSet1, NoSpec)
  end.

%% Calls the annotation on each function in the OpenSet.
annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec) ->
  %% Do it with a tail recursive function.
  annotate_types_helper_pass(FunctionASTS, TSM, OpenSet, NoSpec, []).

annotate_types_helper_pass(FunctionASTS, TSM, [], _NoSpec, OpenSet1) -> {FunctionASTS, TSM, lists:reverse(OpenSet1)};
annotate_types_helper_pass(FunctionASTS, TSM, [{Mfa, Persistence}|Mfas], NoSpec, OpenSet1) ->
  %% Fetch the function AST.
  AST = dict:fetch(Mfa, FunctionASTS),
  %% Fetch the function Spec.
  Spec = dict:fetch(Mfa, TSM),
  %% Add the persistene to the symbol table
  TSMP = merge_all_dicts([TSM, Persistence]),
  %% Run the annotation on the AST.
  {NewAST, D, C, P} = pass_down_fun_types(Mfa, AST, Spec, TSMP, NoSpec),
  %% Update the symbol table and the open set.
  {TSM1, OpenSet2} = update_from_detected(D, TSM, OpenSet1),
  %% If there has been some change in the annotations of the funtion or
  %% calls to functions that do not much the current signature have been detected:
  case C or (length(D) > 0) of
    true -> %% Add the functions that should be traversed again in OpenSet.
      OpenSet3 = update_open_set(Mfa, P, OpenSet2);
    false -> %% Else do not update the OpenSet.
      OpenSet3 = OpenSet2
  end,
  %% Check if the current function is part of the NoSpec set.
  case sets:is_element(Mfa, NoSpec) of
    true -> %% If it is then
      %% fetch its type from its AST.
      T = get_cerl_type(NewAST),
      %% Check if it has been typed succesfully.
      case erl_types:is_erl_type(T) of
        true -> %% If it has then
          %% update its signature in the symbol table.
          [S] = dict:fetch(Mfa, TSM1),
          NewS = erl_types:t_fun(erl_types:t_fun_args(S), T),
          TSM2 = dict:store(Mfa, [NewS], TSM1);
        false -> TSM2 = TSM1
      end;
    false ->
      TSM2 = TSM1
  end,
  %% Update its AST in the dictionary.
  NewASTS = dict:store(Mfa, NewAST, FunctionASTS),
  annotate_types_helper_pass(NewASTS, TSM2, Mfas, NoSpec, OpenSet3).

%% Updates the open set for a specific Mfa.
%% If this Mfa is already in the open set,
%% puts it in the end of the open set, else
%% just append it.
update_open_set(Mfa, P, OpenSet) ->
  lists:reverse([{Mfa, P}|update_open_set1(Mfa, OpenSet, [])]).

update_open_set1(_Mfa, [], Acc) -> Acc;
update_open_set1(Mfa, [{Mfa, _P}|Rest], Acc) ->
  update_open_set1(Mfa, Rest, Acc);
update_open_set1(Mfa, [A|R], Acc) ->
  update_open_set1(Mfa, R, [A|Acc]).

%% Updates the open set and symbol table
%% from detected function calls
%% to functions that are in the NoSpec set.
update_from_detected([], TSM, OpenSet) -> {TSM, OpenSet};
update_from_detected([{Mfa, Spec}|Rest], TSM, OpenSet) ->
  OpenSet1 = [{Mfa, dict:new()}|OpenSet],
  case dict:find(Mfa, TSM) of
    {ok, [Cur]} ->
      TSM1 = dict:store(Mfa, [erl_types:t_sup(Cur, Spec)], TSM);
    error ->
      TSM1 = dict:store(Mfa, [Spec], TSM)
  end,
  update_from_detected(Rest, TSM1, OpenSet1).

%% Finds which functions don't have a signature from the Fset.
find_nospec(FSet, Sigs) ->
  Fn = fun(F) -> not dict:is_key(F, Sigs) end,
  sets:filter(Fn, FSet).

%% Creates the initial open set.
%% The open set is just a queue containing tuples of the form:
%% {Mfa, Persistence}
make_open_set(FSet, Sigs) ->
  Fn = fun(F) ->
           case dict:is_key(F, Sigs) of
             true -> length(dict:fetch(F, Sigs)) =:= 1;
             false -> false
           end
       end,
  O1 = sets:to_list(sets:filter(Fn, FSet)),
  [{X, dict:new()} || X <- O1].

%% ==========================
%% Single function annotation
%% ==========================

%% Returns the type type of a node given its list of annotations.
%% The type could be an erl_type or 'notype'.
get_type([]) -> notype;
get_type([Hd|Tl]) ->
  case Hd of
    {node_type, Value} ->
      Value;
    _ ->
      get_type(Tl)
  end.

%% Same as get_type but it is given the actual cerl node.
-spec get_cerl_type(cerl:cerl()) -> erl_types:erl_type() | notype.
get_cerl_type(T) -> get_type(cerl:get_ann(T)).

%% Updates the type of a cerl node to the given one altering its annotation list.
update_type(Tree, Type) ->
  Anno = cerl:get_ann(Tree),
  cerl:set_ann(Tree, update_type(Anno, Type, [], false)).

update_type([], Type, Acc, false) -> [{node_type, Type}|Acc];
update_type([], _, Acc, true) -> Acc;
update_type([{node_type, _}|T], Type, Acc, _) -> update_type(T, Type, [{node_type, Type}|Acc], true);
update_type([H|T], Type, Acc, Found) -> update_type(T, Type, [H|Acc], Found).

%% Adds the annotation 'type_dependent_unreachable' to the list of annotations
%% of the given cerl node representing a case clause.
mark_as_unreachable(Clause) ->
  Anno = cerl:get_ann(Clause),
  case cuter_graphs:list_contains(type_dependent_unreachable, Anno) of
    false ->
      cerl:add_ann([type_dependent_unreachable], Clause);
    true ->
      Clause
  end.

%% Removes the 'type_dependent_unreachable' annotation from the list of annotations
%% of a case clause.
mark_as_reachable(Clause) ->
  Anno = [T || T <- cerl:get_ann(Clause), T =/= type_dependent_unreachable],
  cerl:set_ann(Clause, Anno).

%% Returns whether the tree node has a type in its annotations.
%% If the tree node has the type 'notype' then it is considered as not having a type.
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

%% Returns a list of types given a list of cerl nodes.
arg_types(Args) ->
  lists:map(fun get_cerl_type/1, Args).

%% Returns a list of argument types of a 'let' node.
%% If the 'let' node is just on one term, it returns a list containint
%% just the type of this term. If it is a 'values' node, it returns a list
%% containing all of the types inside the 'values' node.
let_arg_types(Arg) ->
  case cerl:type(Arg) of
    values ->
      arg_types(cerl:values_es(Arg));
    _ -> [get_cerl_type(Arg)]
  end.

%% Introduces variables Vars in the symbol table TSM with types Types.
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
%% Helper type functions
%% =====================

%% Takes a cerl node representing a pattern in a case clause and
%% creates an equivalent erl_type. It takes TSM2 as an argument,
%% which holds all the variables contained in the guard of the specific
%% clause. If these variables are encountered, they get replaced by the
%% erl_type they are forced to have by the guard. If any other variables
%% are encountered, they are either replaced with erl_types:t_any() if they
%% are not in the symbol table or erl_types:t_none() if they exist in the
%% symbol table.
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

%% Returns the type of the result of a function call
%% given the types of its arguments.
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

%% Just a wrapper of erl_types:t_sup/1 to create a union.
t_union(Types) ->
  erl_types:t_sup(Types).

%% The term unify is not exact in this case.
%% This function takes a cerl node of a clause pattern
%% and an erl_type. Its purpose is to find the values of
%% all variables in the cerl tree, such that if this cerl
%% tree was an erl_type, it would be a valid subtype of
%% Type. Again, TSM is the symbol table and TSM2 holds all the
%% variables encountered in the guard of the clause with their
%% constrained types.
unify_pattern(Tree, TSM, TSM2, Type) ->
  %% We distinguish cases depending on the type of the cerl node Tree.
  case cerl:type(Tree) of
    literal ->
      %% If it is a literal, then we have no variables left to assign types.
      {ok, TSM};
    var ->
      %% If it is a var, we check whether it is in the symbol table.
      case dict:find(cerl:var_name(Tree), TSM) of
        {ok, VarType} ->
          %% If it is, then we check whether its type can be unified
          %% with Type.
          try erl_types:t_unify(VarType, Type) of
            _ -> {ok, TSM}
          catch
            _ -> {error, mismatch}
          end;
        error ->
          %% If it is not, then we check if it is a part of TSM2.
          case dict:find(cerl:var_name(Tree), TSM2) of
            {ok, VarGuardType} ->
              %% If it is, then if it is a supertype of Type, we store it in the symbol table
              case erl_types:t_is_subtype(Type, VarGuardType) of
                true -> {ok, dict:store(cerl:var_name(Tree), VarGuardType, TSM)};
                false -> {error, mismatch}
              end;
            error ->
              %% If it is a new variable, it has to have the type Type.
              {ok, dict:store(cerl:var_name(Tree), Type, TSM)}
          end
      end;
    cons ->
      %% If it is a cons, we check if Type is a list.
      case erl_types:t_is_list(Type) of
        true ->
          %% If it is, we recursively do the same for its values.
          NewType = erl_types:t_nonempty_list(erl_types:t_list_elements(Type)),
          Hdt = unify_pattern(cerl:cons_hd(Tree), TSM, TSM2, erl_types:t_cons_hd(NewType)),
          case Hdt of
            {ok, TSM1} ->
              Tlt = unify_pattern(cerl:cons_tl(Tree), TSM1, TSM2, erl_types:t_cons_tl(NewType)),
              case Tlt of
                {ok, TSMnew} -> {ok, TSMnew};
                _ -> {error, mismatch}
              end;
            _  ->
              {error, mismatch}
          end;
        false ->
          %% If Type is not a list, it might be a union of a list and something else.
          %% In this case, we want to do this procedure for the list part of the union,
          %% which is done with the function below.
          try_to_handle_union(Tree, TSM, TSM2, Type, erl_types:t_list())
      end;
    tuple ->
      %% Tuple is similar to list. We check if Type is also a tuple.
      case erl_types:t_is_tuple(Type) of
        true ->
          %% If it is, we recursively do this procedure for its values.
          Type1 = 
            try erl_types:t_tuple_size(Type) of
              _ -> Type
            catch
              _:_ -> erl_types:t_tuple(length(cerl:tuple_es(Tree)))
            end,
          case length(cerl:tuple_es(Tree)) == erl_types:t_tuple_size(Type1) of
            true -> 
              lists:foldl(
                fun({E, Et}, V) ->
                    case V of
                      {ok, V1} ->
                        unify_pattern(E, V1, TSM2, Et);
                      {error, _} ->
                        {error, mismatch}
                    end
                end,
                {ok, TSM},
                lists:zip(cerl:tuple_es(Tree), erl_types:t_tuple_args(Type1))
               );
            false -> {error, mismatch}
          end;
        false ->
          %% If it is not, it might be a union of a tuple and something else.
          %% Again, we would want to do this procedure for the tuple part of the list.
          try_to_handle_union(Tree, TSM, TSM2, Type, erl_types:t_tuple())
      end;
    _ ->
      %% TODO: maps, bistrings. For now we just return TSM as is. If we don't
      %% add the variables in the symbol table, they won't be constrained, so
      %% we will overaproximate any subsequent types. This might result in
      %% some nodes being annotated with 'maybe_error' equal to true, which
      %% is pessimistic and leaves our analysis sound.
      {ok, TSM}
  end.

%% Checks whether Type is a union. If it is, then it tries
%% to call unify_pattern/4 with the part of the union we
%% are interested in.
try_to_handle_union(Tree, TSM, TSM2, Type, T) ->
  case erl_types:is_erl_type(Type) andalso erl_types:is_erl_type(T) of
    true ->
      H = erl_types:t_subtract(Type, (erl_types:t_subtract(Type, T))),
      case erl_types:t_is_none(H) of
        true -> {error, mismatch};
        false -> unify_pattern(Tree, TSM, TSM2, H)
      end;
    false ->
      {error, mismatch}
  end.

%% ==================
%% Passing down types
%% ==================

%% This is the entry point for annotating the AST
%% of a function with type information in each node.
pass_down_fun_types({M, _F, _A}, AST, Spec, TSM, NoSpec) ->
  pass_down_types_helper(AST, Spec, TSM, M, NoSpec).

pass_down_types_helper(Fun, Spec, TSM, Mod, NoSpec) ->
  TSM2 = put_vars(cerl:fun_vars(Fun), erl_types:t_fun_args(hd(Spec)), TSM),
  {Body, D, C, _DC, P} = pass_down_types(cerl:fun_body(Fun), TSM2, Mod, notype, NoSpec, sets:new()),
  {cerl:update_c_fun(Fun, cerl:fun_vars(Fun), Body), D, C, P}.

%% Core of the analysis.
%% Arguments:
%% - Tree: The AST node under consideration.
%% - TSM: Symbol table. It is a dictionary containint types of functiosn and variables.
%% - Mod: The module in which the function is defined.
%% - ArgType: Holds the type of the argument of the case construct this node is enclosed in (if there is one).
%% - NoSpec: The set containing all the functions that originally had no specification.
%% - Closures: A symbol table-like dictionary for closures defined in the function we are annotating.
%% Returns:
%% 1. The same tree node annotated with type information.
%% 2. A list of tuples of the form {Mfa, erl_type} corresponding to calls to functions encountered 
%%    belonging to NoSpec with their newly calculated types.
%% 3. A boolean value representing whether annotations have changed in the current node or its children.
%%    This is used for the fix-point computation.
%% 4. Similar to 2 but for closures because let_rec nodes do their own fix-point computation.
%% 5. Persistence table. It contains all the variables encountered inside closures so they are appended
%%    to the symbol table if a function is annotated again. This way no infinite loops happen.
%% Most node types don't perform any serious calculation. They just call this function recursively
%% on their children and use combine the results to assign a type on themselves and return the
%% desired values. Some nodes need more logic so they have their own functions.
pass_down_types(Tree, TSM, Mod, ArgType, NoSpec, Closures) ->
  CurType = get_cerl_type(Tree),
  case cerl:type(Tree) of
    alias ->
      {Pat, D1, C1, CD1, P1} = pass_down_types(cerl:alias_pat(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      Var = cerl:alias_var(Tree),
      T = get_cerl_type(Pat),
      Var1 = update_type(Var, T),
      Change = C1 or (CurType =/= T),
      Tree1 = update_type(Tree, T),
      {cerl:update_c_alias(Tree1, Var1, Pat), D1, Change, CD1, P1};
    'apply' ->
      pass_down_types_apply(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    call ->
      pass_down_types_call(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    'case' ->
      pass_down_types_case(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    clause ->
      pass_down_types_clause(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    cons ->
      {Hd, D1, C1, CD1, P1} = pass_down_types(cerl:cons_hd(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Tl, D2, C2, CD2, P2} = pass_down_types(cerl:cons_tl(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      %% Annotate types for head and tail. If at least one of them does not have a type, then
      %% the whole list does not have a type either.
      Tree1 = 
        case {get_cerl_type(Hd), get_cerl_type(Tl)} of
          {X, Y} when X =:= notype orelse Y =:= notype -> update_type(Tree, notype);
          _ -> update_type(Tree, erl_types:t_cons(get_cerl_type(Hd), get_cerl_type(Tl)))
        end,
      Change = C1 or C2 or (CurType =/= get_cerl_type(Tree1)),
      P = merge_all_dicts([P1, P2]),
      {cerl:update_c_cons(Tree1, Hd, Tl), D1 ++ D2, Change, CD1 ++ CD2, P};
    tuple ->
      %% Annotate types for all members of the tuple. If at least one does not have a type,
      %% mark the tuple as having no type as well.
      {Es, D, C, CD, P} = pass_down_types_all(cerl:tuple_es(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      Tree1 =
        case lists:foldl(fun(X, Y) -> Y orelse (get_cerl_type(X) =:= notype) end, false, Es) of
          true ->
            update_type(Tree, notype);
          false -> update_type(Tree, erl_types:t_tuple(lists:map(fun get_cerl_type/1, Es)))
        end,
      Change = C or (CurType =/= get_cerl_type(Tree1)),
      {cerl:update_c_tuple(Tree1, Es), D, Change, CD, P};
    'fun' ->
      pass_down_types_fun(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    'let' ->
      pass_down_types_let(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    letrec ->
      pass_down_types_letrec(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType);
    literal ->
      {update_type(Tree, erl_types:t_from_term(element(3, Tree))), [], false, [], dict:new()};
    seq ->
      %% The type of a seq node is the type of its body.
      {Arg, D1, C1, CD1, P1} = pass_down_types(cerl:seq_arg(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Body, D2, C2, CD2, P2} = pass_down_types(cerl:seq_body(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      Change = C1 or C2 or (CurType =/= get_cerl_type(Body)),
      P = merge_all_dicts([P1, P2]),
      {cerl:update_c_seq(update_type(Tree, get_cerl_type(Body)), Arg, Body), D1 ++ D2, Change, CD1 ++ CD2, P};
    'try' ->
      {Arg, D1, C1, CD1, P1} = pass_down_types(cerl:try_arg(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Vars, D2, C2, CD2, P2} = pass_down_types_all(cerl:try_vars(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Body, D3, C3, CD3, P3} = pass_down_types(cerl:try_body(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Evars, D4, C4, CD4, P4} = pass_down_types_all(cerl:try_evars(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      {Handler, D5, C5, CD5, P5} = pass_down_types(cerl:try_handler(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      Change = C1 or C2 or C3 or C4 or C5 or (CurType =/= get_cerl_type(Body)),
      D = lists:append([D1, D2, D3, D4, D5]),
      CD = lists:append([CD1, CD2, CD3, CD4, CD5]),
      P = merge_all_dicts([P1, P2, P3, P4, P5]),
      {cerl:update_c_try(update_type(Tree, get_cerl_type(Body)), Arg, Vars, Body, Evars, Handler), D, Change, CD, P};
    primop ->
      %% We do not assign a type to a primop because it is considered to produce
      %% errors by the subsequent analysis.
      {update_type(Tree, notype), [], false, [], dict:new()};
    values ->
      %% The type of a values node is a tuple type with elements the types of the children of the values node.
      %% This holds if all of them have a valid type.
      {Es, D1, C1, CD1, P1} = pass_down_types_all(cerl:values_es(Tree), TSM, Mod, ArgType, NoSpec, Closures),
      case lists:all(fun has_type/1, Es) of
        true ->
          {cerl:update_c_values(update_type(Tree, erl_types:t_tuple([get_cerl_type(T) || T <- Es])), Es), D1, C1, CD1, P1};
        false ->
          {cerl:update_c_values(update_type(Tree, notype), Es), D1, C1 or (CurType =/= notype), CD1, P1}
      end;
    var ->
      %% The type of a variable is the type it has been found to have in the symbol table.
      %% If it is not in the symbol table then it has no type.
      case dict:find(cerl:var_name(Tree), TSM) of
        {ok, Type} ->
          {update_type(Tree, Type), [], false, [], dict:new()};
        _ -> {update_type(Tree, notype), [], false, [], dict:new()}
      end;
    _ ->
      %% Catches rest not supported nodes. It annotates them with no type. This
      %% might make nodes that are safe seem unsafe by the subsequent analysis
      %% but not the other way around so we are sound.
      {Tree, [], false, [], dict:new()}
  end.

%% Logic for when the tree is an apply node.
%% Arguments and rerurn values are the same as pass_down_types/6
%% with the current type additionally passed as an argument.
pass_down_types_apply(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  %% Annotate types for arguments of the apply.
  {Args, D1, C1, CD1, P1} = pass_down_types_all(cerl:apply_args(Tree), TSM, Mod, ArgType, NoSpec, Closures),
  %% Fetch the apply operation.
  Op = cerl:apply_op(Tree),
  {Tree1, D2, C2, CD2} =
    %% Check if all arguments have types.
    case lists:all(fun has_type/1, Args) of
      true ->
        %% If they do then distinguish between the possible forms of Op.
        case cerl:type(Op) of
          var ->
            %% If Op is a variable, create the Mfa using the module we are currently in.
            OpN = case cerl:var_name(Op) of {F, A} -> {Mod, F, A}; Name -> Name end,
            %% Search for the function in the symbol table.
            case dict:find(OpN, TSM) of
              {ok, Specs} ->
                %% If it is in the symbol table, fetch the type of the application.
                case application_type(Specs, arg_types(Args)) of
                  {ok, Type} ->
                    %% If it is a valid type, we use this as the type of the apply node.
                    {update_type(Tree, Type), D1, false, CD1};
                  error ->
                    %% Else the function may be in the NoSpec set and we have to update its signature
                    %% to satisfy this call.
                    case sets:is_element(OpN, NoSpec) of
                      true ->
                        %% If it is find its new type.
                        NewSpec = rewrite_spec(arg_types(Args), Specs),
                        {Tree, [{OpN, NewSpec} | D1], true, CD1};
                      false ->
                        %% If it is not, it might be a closure.
                        case sets:is_element(OpN, Closures) of
                          true ->
                            %% If it is, update its spec.
                            NewSpec = rewrite_spec(arg_types(Args), Specs),
                            {Tree, D1, true, [{OpN, NewSpec} | CD1]};
                          false ->
                            %% If it is not, don't update the tree node.
                            {Tree, D1, false, CD1}
                        end
                    end
                end;
              error ->
                %% If it is not in the symbol table, then it might not have a spec.
                case sets:is_element(OpN, NoSpec) of
                  true ->
                    %% If it is in the NoSpec set then compute its new spec.
                    {Tree, [{OpN, erl_types:t_fun(arg_types(Args), erl_types:t_any())} | D1], true, CD1};
                  false ->
                    %% If it is not, it may be a closure.
                    case sets:is_element(OpN, Closures) of
                      true ->
                        %% If it is, calculate its new spec.
                        {Tree, D1, true, [{OpN, erl_types:t_fun(arg_types(Args), erl_types:t_any())} | CD1]};
                      false->
                        %% If it isn't leave the apply node as is.
                        {Tree, D1, false, CD1}
                    end
                end
            end;
          _ ->
            %% This shouldn't be reachable.
            %% If we reach here it means that an apply was made to a variable,
            %% that is not of the form {F, A}.
            error("unhandled op")
        end;
      _ -> {Tree, D1, false, CD1}
    end,
  Change = C1 or C2 or (CurType =/= get_cerl_type(Tree1)),
  {cerl:update_c_apply(Tree1, Op, Args), D2, Change, CD2, P1}.

%% Similar to the logic of the apply node.
%% It follows the same checks but disregards closures
%% since they would be called through an apply node.
pass_down_types_call(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  {Args, D1, C1, CD1, P1} = pass_down_types_all(cerl:call_args(Tree), TSM, Mod, ArgType, NoSpec, Closures),
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
  {cerl:update_c_call(Tree1, ModName, Name, Args), D2, Change, CD1, P1}.

%% Handles case nodes. In case nodes we also have to annotate clauses that
%% are unreachable using the type data with the annotation 'type_dependent_unreachable'.
pass_down_types_case(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  %% Call recursively for argument and clauses.
  {Arg, D1, C1, CD1, P1} = pass_down_types(cerl:case_arg(Tree), TSM, Mod, ArgType, NoSpec, Closures),
  {Clauses1, D2, C2, CD2, P2} = pass_down_types_all(cerl:case_clauses(Tree), TSM, Mod, get_cerl_type(Arg), NoSpec, Closures),
  %% Then mark unreachable clauses.
  Clauses = mark_unreachable_clauses(Clauses1, get_cerl_type(Arg), TSM, Arg),
  Clauses2 = [Clause || Clause <- Clauses, not get_type_dependent_unreachable(Clause)],
  %% The type of the case node is the union of all types of reachable clauses.
  Type = 
    case lists:all(fun has_type/1, Clauses2) of
      true ->
        T = arg_types(Clauses2),
        case cuter_graphs:list_contains(notype, T) of
          true -> notype;
          false -> t_union(T)
        end;
      false ->
        notype
    end,
  Change = C1 or C2 or (CurType =/= Type),
  P = merge_all_dicts([P1, P2]),
  {cerl:update_c_case(update_type(Tree, Type), Arg, Clauses), D1 ++ D2, Change, CD1 ++ CD2, P}.

%% Handles clause nodes.
pass_down_types_clause(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  %% Define a closure to map to all patterns of the clause.
  %% This closure creates the symbol table containing all the variables
  %% constrained by the guard, and calls unify_pattern/4 to create
  %% the updated symbol table for the clause.
  Fn = fun({Pat, AType}, V) ->
           case V of
             {ok, V1} ->
               {A, TSMorT} = update_tsm_from_guard(Tree, V1, []),
               case A of
                 tsm ->
                   unify_pattern(Pat, V1, TSMorT, AType);
                 _ ->
                   unify_pattern(Pat, V1, dict:new(), AType)
               end;
             {error, mismatch} -> {error, mismatch}
           end
       end,
  %% Check if there are more than one pattern.
  case length(cerl:clause_pats(Tree)) > 1 of
    true ->
      %% If there are, check if ArgType is a tuple.
      case erl_types:t_is_tuple(ArgType) of
        true ->
          %% If it is, get the types of the elements of ArgType.
          ATypes = erl_types:t_tuple_args(ArgType),
          %% These should be as many as the patterns.
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
  %% Check if ArgTypes have the same length as the patterns (check for just one pattern).
  %% Then use Fn to create the new symbol table.
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
  %% Use the new symbol table to pass down the types to all children and return normally.
  {Pats, D1, C1, CD1, P1} = pass_down_types_all(cerl:clause_pats(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  {Guard, D2, C2, CD2, P2} = pass_down_types(cerl:clause_guard(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  {Body, D3, C3, CD3, P3} = pass_down_types(cerl:clause_body(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  Change = C1 or C2 or C3 or (CurType =/= get_cerl_type(Body)),
  D = lists:append([D1, D2, D3]),
  CD = lists:append([CD1, CD2, CD3]),
  P = merge_all_dicts([P1, P2, P3]),
  {cerl:update_c_clause(update_type(Tree, get_cerl_type(Body)), Pats, Guard, Body), D, Change, CD, P}.

%% Handles a fun node.
pass_down_types_fun(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  %% Add the function variables to the symbol table.
  TSM1 = put_vars(cerl:fun_vars(Tree), [erl_types:t_any() || _ <- cerl:fun_vars(Tree)], TSM),
  %% Annotate them, we can since they are in the symbol table.
  {Vars, _D1, _C1, _CD1, _P1} = pass_down_types_all(cerl:fun_vars(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  %% Annotate the body.
  {Body, D1, C1, CD1, P1} = pass_down_types(cerl:fun_body(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  %% Create the new function type of the node.
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
  {cerl:update_c_fun(Tree1, Vars, Body), D1, Change, CD1, P1}.

%% Handles a let node.
pass_down_types_let(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  %% Find the types of the assigned values to variables.
  {Arg, D1, C1, CD1, P1} = pass_down_types(cerl:let_arg(Tree), TSM, Mod, ArgType, NoSpec, Closures),
  %% Put the variables to the symbol table.
  TSM1 = put_vars(cerl:let_vars(Tree), let_arg_types(Arg), TSM),
  {Vars, D2, C2, CD2, P2} = pass_down_types_all(cerl:let_vars(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  {Body, D3, C3, CD3, P3} = pass_down_types(cerl:let_body(Tree), TSM1, Mod, ArgType, NoSpec, Closures),
  %% The type of a let node is the type of its body.
  Tree1 =
    case has_type(Body) of
      true ->
        update_type(Tree, get_cerl_type(Body));
      false ->
        update_type(Tree, notype)
    end,
  Change = C1 or C2 or C3 or (CurType =/= get_cerl_type(Tree1)),
  D = lists:append([D1, D2, D3]),
  CD = lists:append([CD1, CD2, CD3]),
  P = merge_all_dicts([P1, P2, P3]),
  {cerl:update_c_let(Tree1, Vars, Arg, Body), D, Change, CD, P}.

%% Handles a letrec node.
%% Since this node introduces closures, we need to do a fix-point computation
%% Similar to the top one for all functions. That is because closures initially
%% don't have a signature since the programmer does not provide one. They also
%% can be recursive in case they are compiler generated for list comprehensions.
pass_down_types_letrec(Tree, TSM, Mod, ArgType, NoSpec, Closures, CurType) ->
  {Names, Funsb} = lists:unzip(cerl:letrec_defs(Tree)),
  FunNames = [cerl:var_name(Name) || Name <- Names],
  FunNames1 = sets:from_list([{Mod, F, A} || {F, A} <- FunNames]),
  %% Add the new closures to the set of closures.
  NewClosures = sets:union(Closures, FunNames1),
  %% Do the fix-point computation.
  {Funs, Body, D, C, CD, TSM1} = pass_down_types_letrec_fix(Names, Funsb, cerl:letrec_body(Tree), TSM, Mod, ArgType, NoSpec, NewClosures),
  %% Filter out all functions that are introduced in other letrec ndoes to create
  FilterFun = fun(Key, _Value) -> sets:is_element(Key, FunNames1) end,
  %% The peristence of the closures in this letrec node.
  Persistence = dict:filter(FilterFun, TSM1),
  Change = C or (CurType =/= get_cerl_type(Body)),
  %% The type of the letrec node is the type of its body.
  {cerl:update_c_letrec(update_type(Tree, get_cerl_type(Body)), lists:zip(Names, Funs), Body), D, Change, CD, Persistence}.

%% Fix point computation for letrec nodes.
pass_down_types_letrec_fix(Names, Funsb, Body, TSM, Mod, ArgType, NoSpec, Closures) ->
  FunNames1 = [cerl:var_name(Name) || Name <- Names],
  FunNames2 = [{Mod, F, A} || {F, A} <- FunNames1],
  FunNames = sets:from_list(FunNames2),
  %% Do a pass through the functions.
  {Funs, D1, C1, CD1} = pass_down_types_letrec_fix_pass(FunNames2, Funsb, TSM, Mod, ArgType, NoSpec, Closures, []),
  %% Do a pass through the body.
  {Body1, D2, C2, CD2, _P2} = pass_down_types(Body, TSM, Mod, ArgType, NoSpec, Closures),
  CD = CD1 ++ CD2,
  %% Find all detected closure calls to closures that do not have yet a signature
  %% or need their signature updated that belong to this letrec node.
  RelevantCD = [D || {OpN, _NewSpec}=D <- CD, sets:is_element(OpN, FunNames)],
  %% If there are none, we are done, else we continue the fix-point.
  case length(RelevantCD) of
    0 ->
      RestCD = [D || {OpN, _NewSpec}=D <- CD, not sets:is_element(OpN, FunNames)],
      {Funs, Body1, D1 ++ D2, C1 or C2, RestCD, TSM};
    _ ->
      {TSM1, _} = update_from_detected(RelevantCD, TSM, []),
      pass_down_types_letrec_fix(Names, Funs, Body1, TSM1, Mod, ArgType, NoSpec, Closures)
  end.

%% Performs a pass through the functions introduced in a letrec node.
pass_down_types_letrec_fix_pass([], _Funsb, _TSM, _Mod, _ArgType, _NoSpec, _Closures, Acc) ->
  {Funs, D, C, CD, _P} = unzip5(Acc),
  {lists:reverse(Funs), lists:append(D), lists:foldl(fun erlang:'or'/2, false, C), lists:append(CD)};
pass_down_types_letrec_fix_pass([Name|Names], [Funb|Funsb], TSM, Mod, ArgType, NoSpec, Closures, Acc) ->
  case dict:find(Name, TSM) of
    {ok, [Spec]} ->
      TSM1 = put_vars(cerl:fun_vars(Funb), erl_types:t_fun_args(Spec), TSM);
    error ->
      TSM1 = TSM
  end,
  {Args, _, _, _, _} = pass_down_types_all(cerl:fun_vars(Funb), TSM1, Mod, ArgType, NoSpec, Closures),
  {Body, D, C, CD, P} = pass_down_types(cerl:fun_body(Funb), TSM1, Mod, ArgType, NoSpec, Closures),
  Fun = cerl:update_c_fun(Funb, Args, Body),  
  pass_down_types_letrec_fix_pass(Names, Funsb, TSM, Mod, ArgType, NoSpec, Closures, [{Fun, D, C, CD, P}|Acc]).

%% Wrapper to call pass_down_types/6 to a list of nodes.
pass_down_types_all(Trees, TSM, Mod, ArgType, NoSpec, Closures) ->
  R = lists:map(fun(A) -> pass_down_types(A, TSM, Mod, ArgType, NoSpec, Closures) end, Trees),
  {NewTrees, AllDetected, Changes, ClosuresDetected, Persistence} = unzip5(R),
  {NewTrees, lists:append(AllDetected), lists:foldl(fun erlang:'or'/2, false, Changes), lists:append(ClosuresDetected), merge_all_dicts(Persistence)}.

%% Unzips a list of tuples of length 5.
unzip5(L) -> unzip5(L, [], [], [], [], []).

unzip5([], Acc1, Acc2, Acc3, Acc4, Acc5) -> 
  {lists:reverse(Acc1),
   lists:reverse(Acc2),
   lists:reverse(Acc3),
   lists:reverse(Acc4),
   lists:reverse(Acc5)};
unzip5([{A, B, C, D, E}|Rest], Acc1, Acc2, Acc3, Acc4, Acc5) ->
  unzip5(Rest, [A|Acc1], [B|Acc2], [C|Acc3], [D|Acc4], [E|Acc5]).

%% Rewrites a function signature with updated argument types.
rewrite_spec(ArgTypes, [Spec]) ->
  SupArgs = fun({A, B}) -> erl_types:t_sup(A, B) end,
  ArgTypes1 = lists:map(SupArgs, lists:zip(ArgTypes, erl_types:t_fun_args(Spec))),
  erl_types:t_fun(ArgTypes1, erl_types:t_fun_range(Spec)).

%% ===========================
%% Marking unreachable clauses
%% ===========================

%% This is the entry point to the logic that marks the unreachable clauses of a case.
%% Arguments:
%% - Clauses: List of cerl nodes of type clause.
%% - ArgType: The type of the case argument these clauses belong to.
%% - TSM: Symbol table.
%% - Arg: Case argument.
%% Returns:
%% The list of clauses annotated with 'type_dependent_unreachable' if they can't be reached.
mark_unreachable_clauses(Clauses, ArgType, TSM, Arg) ->
  %% Check if Arg is a values node to create the Argument list correctly.
  case cerl:type(Arg) =:= values of
    true -> ArgList = cerl:values_es(Arg);
    false -> ArgList = [Arg]
  end,
  %% If we haven't computed a type for the argument,
  %% we can not know if a clause is unreachable, so
  %% just return clauses as they are.
  case ArgType =:= notype of
    false -> mark_unreachable_clauses(Clauses, ArgType, TSM, ArgList, []);
    true -> Clauses
  end.

%% Tail-recursive so reverse the clauses when returning to maintain their order.
mark_unreachable_clauses([], _, _, _, NewClauses) -> lists:reverse(NewClauses);
mark_unreachable_clauses([Clause|Clauses], ArgType, TSM, Arg, NewClauses) ->
  Pats =  cerl:clause_pats(Clause),
  %% If the ArgType is None, we have subtracted everything from the original argument type.
  %% This means that no further clauses can be reached, including this one.
  NewClause =
    case erl_types:t_is_none(ArgType) of
      true -> mark_as_unreachable(Clause);
      false -> mark_as_reachable(Clause)
    end,
  %% Closure to try to subtract two types, and just return the first one
  %% intact in the case of failure.
  SafeSub = fun(A, B) ->
                try erl_types:t_subtract(A, B) of
                  T -> T
                catch
                  _:_ -> A
                end
            end,
  %% Update the symbol table from the clause guard.
  {A, TSMorT} = update_tsm_from_guard(Clause, TSM, Arg),
  %% Distinguish between the return values of update_tsm_from_guard/3.
  case A of
    {argtype, ArgName} ->
      %% We only have a constraint for an argument
      PatTypes1 = lists:map(fun (X) -> t_from_pattern(X, TSM, dict:new()) end, Pats),
      %% Gather the patterns that have a type and convert them to erl_types.
      PatTypes = [PatType || PatType <- PatTypes1, PatType =/= notype],
      %% Check if we could do that for all of them.
      case length(PatTypes) =:= length(Arg) of
        true ->
          %% We could, so replace the variable with its constraint in the patterns.
          %% And then make the new type by subtracting to the current ArgType.
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
      %% We have an updated symbol table.
      %% Do the same as the previous case using the new symbol table.
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

%% Replaces an argument with the type it is constrained to in the patterns.
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

%% A valid guard is a guard we can currently parse to extract useful information
%% that helps us locate the unreachable clauses in a case construct.
valid_guard(Clause, TSM, ArgList) ->
  Guard = cerl:clause_guard(Clause),
  case cerl:type(Guard) of
    literal when element(3, Guard) =:= true -> true;
    call ->
      Args = cerl:call_args(Guard),
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

%% Returns the Mfa called from a guard.
get_call_mfa(Guard) ->
  ModName = cerl:call_module(Guard),
  Name = cerl:call_name(Guard),
  Arity = length(cerl:call_args(Guard)),
  case cerl:type(ModName) =:= literal andalso cerl:type(Name) =:= literal of
    true -> {element(3, ModName), element(3, Name), Arity};
    false -> unmatched
  end.

%% Returns whether X is a variable that is not in the symbol table or
%% in the arguent list of a case construct.
is_unknown_var(X, TSM, ArgList) ->
  case cerl:type(X) of
    var ->
      ArgVarNames = [cerl:var_name(Var) || Var <- ArgList, cerl:type(Var) =:= var],
      case dict:find(cerl:var_name(X), TSM) of
        {ok, _} -> cuter_graphs:list_contains(cerl:var_name(X), ArgVarNames);
        error -> true
      end;
    _ -> false
  end.

%% A 'right' call is a call we support for guards.
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

%% This function looks at the guard of a clause.
%% If the guard is of the form is_'type'(X), it returns the constraint
%% that variable X has the type 'type'.
update_tsm_from_guard(Clause, TSM, ArgList) ->
  case valid_guard(Clause, TSM, ArgList) of
    true ->
      Guard = cerl:clause_guard(Clause),
      case cerl:type(Guard) of
        literal when element(3, Guard) =:= true -> {tsm, dict:new()};
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

%% Decides whether to return a new symbol table,
%% or just a variable constraint.
update_tsm_from_guard_helper(Args, ArgList, Type) ->
  FunArgName = cerl:var_name(hd(Args)),
  ArgVarNames = [cerl:var_name(Var) || Var <- ArgList, cerl:type(Var) =:= var],
  case cuter_graphs:list_contains(FunArgName, ArgVarNames) of
    true -> {{argtype, FunArgName}, Type};
    _ -> {tsm, dict:store(FunArgName, Type, dict:new())}
  end.

%% Fetches the 'type_dependent_unreachable' annotation out of an annotation list.
get_ann_type_dependent_unreachable([]) -> false;
get_ann_type_dependent_unreachable([Hd|Tl]) ->
  case Hd of
    type_dependent_unreachable ->
      true;
    _ ->
      get_ann_type_dependent_unreachable(Tl)
  end.

%% Fetches the 'type_dependent_unreachable' annotation of a cerl node.
-spec get_type_dependent_unreachable(cerl:cerl()) -> boolean().
get_type_dependent_unreachable(T) -> get_ann_type_dependent_unreachable(cerl:get_ann(T)).

%% Merges all dictionaries in a list.
%% It hypothesizes that no two keys are the same.
merge_all_dicts(D) ->
  F = fun(_Key, Value1, _Value2) -> Value1 end,
  F1 = fun(D1, D2) -> dict:merge(F, D1, D2) end,
  lists:foldl(F1, dict:new(), D).
