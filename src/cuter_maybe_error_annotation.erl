-module(cuter_maybe_error_annotation).
%-export([annotate_maybe_error/1, annotate_maybe_error/2, get_maybe_error/1, get_maybe_error_anno/1]).
-export([get_maybe_error/1, get_maybe_error_anno/1]).
%-export_type([maybe_error/0, symbol_table/0]).
-export_type([maybe_error/0]).

-type maybe_error() :: atom().
%-type symbol_table() :: dict:dict().
%-type module_defs() :: [{cerl:cerl(), cerl:cerl()}].

%% annotating function

%-spec annotate_maybe_error(cerl:cerl()) -> cerl:cerl().
%annotate_maybe_error(T) ->
%  io:format("~p~n", [T]),
%  Defs = cerl:module_defs(T),
%  ST = dict:new(),
%  NewDefs = annotate_maybe_error_functions(Defs, ST),
%  cerl:update_c_module(T, cerl:module_name(T), cerl:module_exports(T), cerl:module_attrs(T), NewDefs).
%
%-spec annotate_maybe_error_functions(module_defs(), symbol_table()) -> module_defs().
%annotate_maybe_error_functions(Funs, ST) ->
%  {NewST, NewFuns, Change} = annotate_maybe_error_functions(Funs, ST, [], false),
%  case Change of
%    true ->
%      annotate_maybe_error_functions(NewFuns, NewST);
%    false ->
%      NewFuns
%  end.
%
%-spec annotate_maybe_error_functions(module_defs(), symbol_table(), module_defs(), atom()) -> {symbol_table(), module_defs(), atom()}.
%annotate_maybe_error_functions([], ST, Acc, Change) ->
%  {ST, Acc, Change};
%annotate_maybe_error_functions([{Var, Fun}|T], ST, Acc, Change) ->
%  {Annotated, C} = annotate_maybe_error(Fun, ST),
%  NewST = dict:store(cerl:var_name(Var), {get_maybe_error(Annotated), 'fun'}, ST),
%  annotate_maybe_error_functions(T, NewST, [{Var, Annotated}|Acc], Change or C).
%  
%-spec update_ann(cerl:cerl(), maybe_error()) -> cerl:cerl().
%update_ann(T, Maybe_Error) ->
%  Anno = cerl:get_ann(T),
%  cerl:set_ann(T, update_ann(Anno, Maybe_Error, [], false)).
%
%-spec update_ann([any()], maybe_error(), [any()], atom()) -> [any()].
%update_ann([], Maybe_Error, Acc, false) -> [{maybe_error, Maybe_Error}|Acc];
%update_ann([], _, Acc, true) -> Acc; 
%update_ann([{maybe_error, _}|T], Maybe_Error, Acc, _) -> update_ann(T, Maybe_Error, [{maybe_error, Maybe_Error}|Acc], true); 
%update_ann([H|T], Maybe_Error, Acc, Found) -> update_ann(T, Maybe_Error, [H|Acc], Found).
%  
%-spec put_vars(symbol_table(), [{maybe_error(), atom()}], [cerl:cerl()]) -> symbol_table().
%put_vars(Vars, Flags, SM) ->
%  lists:foldl(fun({Var, Flag}, B) -> dict:store(cerl:var_name(Var), Flag, B) end, SM, lists:zip(Vars, Flags)).
%
%-spec annotate_maybe_error(cerl:cerl(), symbol_table()) -> {cerl:cerl(), atom()}.
%annotate_maybe_error(Tree, SM) ->
%  CurMaybe_Error = get_maybe_error(Tree),
%  case cerl:type(Tree) of
%%    alias ->
%    'apply' ->
%      Op = cerl:apply_op(Tree),
%      {Op1, C1} = case cerl:type(Op) of
%		   var ->
%		     case dict:find(cerl:var_name(Op), SM) of
%		       {ok, {Value, 'fun'}} ->
%			 {update_ann(Op, Value), Value =/= CurMaybe_Error};
%		       _ ->
%			 {update_ann(Op, true), true =/= CurMaybe_Error}
%		     end;
%		   _ ->
%		      io:format("unhandled op")
%		 end,
%      {Args, C2} = annotate_maybe_error_all(cerl:apply_args(Tree), SM),
%      NewMaybe_Error = get_maybe_error(Op1) or get_all_maybe_error(Args),
%      {cerl:update_c_apply(update_ann(Tree, NewMaybe_Error), Op1, Args), C1 or C2};
%%    binary ->
%%    bitstr ->
%    call ->
%      Mod = cerl:call_module(Tree),
%      Name = cerl:call_name(Tree),
%      {NewAnn, C1} = case cerl:is_literal(Mod) andalso cerl:is_literal(Mod) of
%		      true ->
%			case dict:find({Mod, Name}, SM) of
%			  {ok, {Value, 'fun'}} ->
%			    {Value, Value =/= CurMaybe_Error};
%			  _ ->
%			    {true, true =/= CurMaybe_Error}
%			end;
%		      _ -> throw("Unsupported call")
%      end,
%      {Args, C2} = annotate_maybe_error_all(cerl:call_args(Tree), SM),
%      NewMaybe_Error = NewAnn or get_all_maybe_error(Args),
%      {cerl:update_c_call(update_ann(Tree, NewMaybe_Error), Mod, Name, Args), C1 or C2};
%    'case' ->
%      {Arg, C1} = annotate_maybe_error(cerl:case_arg(Tree), SM),
%      {Clauses, C2} = annotate_maybe_error_all(cerl:case_clauses(Tree), SM),
%      NewMaybe_Error = get_maybe_error(Arg) or get_all_maybe_error(Clauses),
%      {cerl:update_c_case(update_ann(Tree, NewMaybe_Error), Arg, Clauses), C1 or C2};
%%    'catch' ->
%    clause ->
%      VarPats = [A || A <- cerl:clause_pats(Tree), cerl:type(A) == var],
%      SM1 = put_vars(VarPats, [{false, var} || _ <- VarPats], SM),
%      {Pats, C1} = annotate_maybe_error_all(cerl:clause_pats(Tree), SM1),
%      {Guard, C2} = annotate_maybe_error(cerl:clause_guard(Tree), SM1),
%      {Body, C3} = annotate_maybe_error(cerl:clause_body(Tree), SM1),
%      NewMaybe_Error = get_maybe_error(Body) or get_all_maybe_error(Pats) or get_maybe_error(Guard),
%      {cerl:update_c_clause(update_ann(Tree, NewMaybe_Error), Pats, Guard, Body), C1 or C2 or C3};
%%    cons ->
%    'fun' ->
%      SM1 = put_vars(cerl:fun_vars(Tree), [{false, var} || _ <- cerl:fun_vars(Tree)], SM),
%      {Vars, C1} = annotate_maybe_error_all(cerl:fun_vars(Tree), SM1),
%      {Body, C2} = annotate_maybe_error(cerl:fun_body(Tree), SM1),
%      NewMaybe_Error = get_maybe_error(Body) or get_all_maybe_error(Vars),
%      {cerl:update_c_fun(update_ann(Tree, NewMaybe_Error), Vars, Body), C1 or C2};
%    'let' ->
%      {Arg, C2} = annotate_maybe_error(cerl:let_arg(Tree), SM),
%      SM1 = put_vars(cerl:let_vars(Tree), get_arg_maybe_errors(Arg), SM),
%      {Vars, C1} = annotate_maybe_error_all(cerl:let_vars(Tree), SM1),
%      {Body, C3} = annotate_maybe_error(cerl:let_body(Tree), SM1),
%      NewMaybe_Error = get_all_maybe_error(Vars) or get_maybe_error(Arg) or get_maybe_error(Body),
%      {cerl:update_c_let(update_ann(Tree, NewMaybe_Error), Vars, Arg, Body), C1 or C2 or C3};
%    letrec ->
%      Combine = fun(X, Y) -> {annotate_maybe_error(X, SM), annotate_maybe_error(Y, SM)} end,
%      Defs1 = [Combine(N, D) || {N, D} <- cerl:letrec_defs(Tree)],
%      Defs = [{element(1, N), element(1, D)} || {N, D} <- Defs1],
%      C1 = lists:foldl(fun (A,B) -> A or B end, false, [element(2, N) or element(2, D) || {N, D} <- Defs1]),
%      {Body, C2} = annotate_maybe_error(cerl:letrec_body(Tree), SM),
%      NewMaybe_Error = lists:foldl(fun (A, B) -> A or B end, false, [get_maybe_error(N) or get_maybe_error(D) || {N, D} <- Defs]) or get_maybe_error(Body),
%      {cerl:update_c_letrec(update_ann(Tree, NewMaybe_Error), Defs, Body), C1 or C2};
%    literal ->
%      {update_ann(Tree, false), true == CurMaybe_Error};
%    primop ->
%      {update_ann(Tree, true), false == CurMaybe_Error};
%%    'receive' ->
%%    seq ->
%%    'try' ->
%%    tuple ->
%%    values ->
%    var ->
%      case dict:find(cerl:var_name(Tree), SM) of
%	{ok, {Value, _}} ->
%	  {update_ann(Tree, Value), Value =/= CurMaybe_Error};
%	error ->
%	  {update_ann(Tree, true), true =/= CurMaybe_Error}
%      end
%  end.
%
%-spec get_arg_maybe_errors(cerl:cerl()) -> [{maybe_error(), atom()}].
%get_arg_maybe_errors(Arg) ->
%  [{get_maybe_error(Arg), var}].
%
%-spec annotate_maybe_error_all([cerl:cerl()], symbol_table()) -> {[cerl:cerl()], atom()}.
%annotate_maybe_error_all(Trees, SM) ->
%  X = [annotate_maybe_error(T, SM) || T <- Trees],
%  {[element(1, Y) || Y <- X], lists:foldl(fun(A, B) -> B or element(2, A) end, false, X)}.
%
-spec get_maybe_error(cerl:cerl()) -> atom().
get_maybe_error(Tree) ->
  Anno = cerl:get_ann(Tree),
  get_maybe_error_anno(Anno).

-spec get_maybe_error_anno([]) -> maybe_error().
get_maybe_error_anno(Anno) ->
  Sf = fun(A, B) ->
	   case A of
	     {maybe_error, T} -> B or T;
	     _ -> B
	   end
       end,
  lists:foldl(Sf, false, Anno).

%-spec get_all_maybe_error([cerl:cerl()]) -> atom().
%get_all_maybe_error(Trees) ->
%  lists:foldl(fun(X,Y) -> X or Y end, false, [get_maybe_error(T) || T <- Trees]).
