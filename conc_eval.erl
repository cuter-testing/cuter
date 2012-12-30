-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").
-include("conc_lib.hrl").

%%--------------------------------------------------------------------------
%% eval(FunInfo, A, Mode, CallType, CodeServer, TraceServer, Register) -> Val
%%   FunInfo :: {named, M, F} | {lambda, Closure}
%%    M :: atom()
%%    F :: atom()
%%    Closure :: fun()
%%   A :: [term()]
%%   Mode :: concrete | symbolic
%%   CallType :: local | external
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   CallerPid :: pid()
%%    Parent :: pid()
%%   Val :: term()
%% Interpret and trace Core Erlang ASTs (concretely or symbolically)
%%--------------------------------------------------------------------------

%% Concrete Evaluation of MFA

%% Handle spawn/1, spawn/2, spawn/3, spawn/4,
%% spawn_link/1 spawn_link/2, spawn_link/3, spawn_link/4
eval({named, erlang, F}, A, concrete, _CallType, CodeServer, TraceServer, CallerPid)
  when F =:= spawn; F =:= spawn_link ->
    register_to_trace(TraceServer, CallerPid),
    send_trace(TraceServer, {func_in, erlang, F, A}), %% Trace
    Result =
      case A of
        [Fun] ->
          EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer, self()],
          erlang:F(conc_eval, eval, EvalArgs);
        [Node, Fun] ->
          EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer, self()],
          erlang:F(Node, conc_eval, eval, EvalArgs);
        [Mod, Fun, Args] ->
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, Mod, Fun}, Args, concrete, Call, CodeServer, TraceServer, self()],
          erlang:F(conc_eval, eval, EvalArgs);
        [Node, Mod, Fun, Args] ->
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, Mod, Fun}, Args, concrete, Call, CodeServer, TraceServer, self()],
          erlang:F(Node, conc_eval, eval, EvalArgs);
        _ ->
          exception(error, undef_function)
      end,
    send_trace(TraceServer, {func_out, erlang, F, A, Result}), %% Trace
    Result;
    
%% Handle spawn_monitor/1, spawn_monitor/3
eval({named, erlang, spawn_monitor}, A, concrete, _CallType, CodeServer, TraceServer, CallerPid) ->
  register_to_trace(TraceServer, CallerPid),
  send_trace(TraceServer, {func_in, erlang, spawn_monitor, A}), %% Trace
  EvalArgs =
    case A of
      [Fun] ->
        [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer, self()];
      [Mod, Fun, Args] ->
        Call = find_call_type(erlang, Mod),
        [{named, Mod, Fun}, Args, concrete, Call, CodeServer, TraceServer, self()];
      _ ->
        exception(error, undef_function)
    end,
  Result = erlang:spawn_monitor(conc_eval, eval, EvalArgs),
  send_trace(TraceServer, {func_out, erlang, spawn_monitor, A, Result}), %% Trace
  Result;
  
%% Handle spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5
eval({named, erlang, spawn_opt}, A, concrete, _CallType, CodeServer, TraceServer, CallerPid) ->
  register_to_trace(TraceServer, CallerPid),
  send_trace(TraceServer, {func_in, erlang, spawn_opt, A}), %% Trace
  Result =
    case A of
      [Fun, Opts] ->
        EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer, self()],
        erlang:spawn_opt(conc_eval, eval, EvalArgs, Opts);
      [Node, Fun, Opts] ->
        EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer, self()],
        erlang:spawn_opt(Node, conc_eval, eval, EvalArgs, Opts);
      [Mod, Fun, Args, Opts] ->
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, Mod, Fun}, Args, concrete, Call, CodeServer, TraceServer, self()],
        erlang:spawn_opt(conc_eval, eval, EvalArgs, Opts);
      [Node, Mod, Fun, Args, Opts] ->
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, Mod, Fun}, Args, concrete, Call, CodeServer, TraceServer, self()],
        erlang:spawn_opt(Node, conc_eval, eval, EvalArgs, Opts);
      _ ->
        exception(error, undef_function)
    end,
  send_trace(TraceServer, {func_out, erlang, spawn_opt, A, Result}), %% Trace
  Result;

eval({named, M, F}, A, concrete, CallType, CodeServer, TraceServer, CallerPid) ->
  register_to_trace(TraceServer, CallerPid),
  send_trace(TraceServer, {func_in, M, F, A}), %% Trace
  %% TODO Some kind of caching instead of constantly querying CodeServer
  case get_module_db(M, CodeServer) of
    unloadable ->
      Result = apply(M, F, A),
      send_trace(TraceServer, {func_out, M, F, A, Result}), %% Trace
      Result;
    {ok, ModDb} ->
    
      %% Get function info
      Arity = length(A),
      Key = {M, F, Arity},
      {Def, Exported} = retrieve_function(Key, ModDb),
      
      %% Check if CallType is compatible
      case check_exported(Exported, CallType) of
        false ->
          %% External function call that is not exported
          exception('error', not_exported_function);
        true ->
          %% Wrap Args into semantic values
          SemanticArgs = conc_lib:terms_to_semantics(A),
          %% Bind function's parameters to the Arguments
%          io:format("[eval]: Def=~n~p~n", [Def]),
          Environment = init_fun_parameters(SemanticArgs, Def#c_fun.vars),
          SemanticResult = eval_expr(M, concrete, CodeServer, TraceServer, Def#c_fun.body, Environment),
          Result = conc_lib:semantic_to_term(SemanticResult),
          send_trace(TraceServer, {func_out, M, F, A, Result}), %% Trace
          Result
      end
  end;
  
eval({lambda, Closure}, A, concrete, _CallType, _CodeServer, TraceServer, CallerPid) ->
  register_to_trace(TraceServer, CallerPid),
  send_trace(TraceServer, {func_in_anon, A}), %% Trace
  SemanticArgs = conc_lib:terms_to_semantics(A),
  %% No need to bind func params, will be done automatically
  SemanticResult = apply(Closure, SemanticArgs),
  Result = conc_lib:semantic_to_term(SemanticResult),
  send_trace(TraceServer, {func_out_anon, A, Result}), %% Trace
  Result.

%% Ensure that function exists
retrieve_function(FuncKey, ModDb) ->
  case ets:lookup(ModDb, FuncKey) of
    [] ->
      exception(error, undef_function);
    [{FuncKey, Val}] ->
      Val
  end.
  
%% Register self() and Parent to the Trace Server
register_to_trace(TraceServer, CallerPid) ->
  case CallerPid =:= self() of
    true ->
      ok;
    false ->
      gen_server:call(TraceServer, {register_parent, CallerPid})
  end.

get_module_db(Mod, CodeServer) ->
  case gen_server:call(CodeServer, {is_stored, Mod}) of
    unloadable ->
      %% Black Box Tested Module
      unloadable;
    false ->
      %% Module Code not loaded yet
      {ok, Mod, ModDb} = gen_server:call(CodeServer, {load, Mod}),
      {ok, ModDb};
    {true, ModDb} ->
      %% Nodule Code loaded
      {ok, ModDb}
  end.

check_exported(true, _CallType) -> true;
check_exported(false, local)    -> true;
check_exported(false, external) -> false.
  
%% Creates a new Environment where function's paremeters
%% are bound to the actual arguments
init_fun_parameters(Args, Vars_c) ->
  Env = conc_lib:new_environment(),
  bind_parameters(Args, Vars_c, Env).
  
bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var_c|Vars_c], Env) ->
  NewEnv = conc_lib:add_binding(Var_c#c_var.name, Arg, Env),
  bind_parameters(Args, Vars_c, NewEnv).
  
%% Send Trace Data to Trace Server
send_trace(TraceServer, Msg) ->
  gen_server:cast(TraceServer, {trace, self(), Msg}).
  
%%--------------------------------------------------------------------
%% exception(Class, Reason)
%%   Class :: error | exit | throw
%%   Reason :: term()
%% Get the information about the error and then raise the exception.
%%--------------------------------------------------------------------
exception(Class, Reason) ->
  erlang:Class(Reason).

%%--------------------------------------------------------------------
%% eval_expr(M, Mode, CodeServer, TraceServer, Expr, Env) -> Sem
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Expr :: cerl()
%%   Env :: conc_lib:environment()
%%   Sem :: #semantic{}
%% Evaluate Core Erlang ASTs in record format
%%--------------------------------------------------------------------

%% c_apply
eval_expr(M, concrete, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Env) ->
  %% Evaluate Op
  {semantic, Op_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Op, Env),
  %% Evaluate Args
  Args_Val = lists:map(
    fun(Arg) ->
      {semantic, Arg_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
      Arg_Val
    end,
    Args
  ),
%  %% DEBUG ---------------------------------------------
%  io:format("[c_apply]: Eval_Op = ~p~n", [Eval_Op]),
%  io:format("[c_apply]: Eval_Args = ~p~n", [Eval_Args]),
%  %% ---------------------------------------------------
  Result = 
    case Op_Val of
      {Fun, _Arity} -> 
        %% Call the local function
        eval({named, M, Fun}, Args_Val, concrete, local, CodeServer, TraceServer, self());
      Closure ->
        %% Apply the evaluated Args to the closure
  %      %% DEBUG --------------------------------------------------------------
  %      io:format("[c_apply]: Closure = ~p, Args = ~p~n", [Closure, Args_Val]),
  %      %% --------------------------------------------------------------------
        eval({lambda, Closure}, Args_Val, concrete, local, CodeServer, TraceServer, self())
    end,
  %% Wrap the result to a semantic value
  conc_lib:term_to_semantic(Result);
  
%% c_call
eval_expr(M, concrete, CodeServer, TraceServer, {c_call, _Anno, Mod, Fun, Args}, Env) ->
  %% Evaluate Module name
  {semantic, ModVal, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Mod, Env),
  %% Evaluate Function name
  {semantic, FunVal, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Fun, Env),
  %% Evaluate Args
  ArgsVal = lists:map(
    fun(Arg) ->
      {semantic, ArgVal, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
      ArgVal
    end,
    Args
  ),
  %% Call MFA
  Result = eval({named, ModVal, FunVal}, ArgsVal, concrete, find_call_type(M, ModVal), CodeServer, TraceServer, self()),
  %% Wrap the result in a semantic object
  #semantic{value=Result, degree=1};
  
%% c_case
eval_expr(M, concrete, CodeServer, TraceServer, {c_case, _Anno, Arg, Clauses}, Env) ->
  Arg_Val = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
%  %% DEBUG -----------------------------------------------------------------
%  io:format("[c_case]: Expr to be matched : ~p~n", [Arg_Val]),
%  %% -----------------------------------------------------------------------
  {Body, NewEnv} = find_clause(M, concrete, CodeServer, TraceServer, Clauses, Arg_Val, Env),
%  %% DEBUG -----------------------------------------------------------------
%  io:format("[c_case]: Will evaluate ~n~p~n in Env : ~p~n", [Body, NewEnv]),
%  %% -----------------------------------------------------------------------
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
  
%% c_catch
eval_expr(M, concrete, CodeServer, TraceServer, {c_catch, _Anno, Body}, Env) ->
  try
    eval_expr(M, concrete, CodeServer, TraceServer, Body, Env)
  catch
    throw:Throw ->
      #semantic{value=Throw, degree=1};
    exit:Exit ->
      #semantic{value={'EXIT', Exit}, degree=1};
    error:Error ->
      #semantic{value={'EXIT', {Error, []}}, degree=1}
  end;

%% c_cons
eval_expr(M, concrete, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Env) ->
  %% Evaluate the head of the list
  {semantic, Hd_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Hd, Env),
  %% Evaluate the tail of the list
  {semantic, Tl_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Tl, Env),
  %% Apply the list constructor
  #semantic{value=[Hd_Val | Tl_Val], degree=1};
  
%% c_fun
eval_expr(M, concrete, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Env) ->
  %% Manually creating anonymous func
  %% and not use a list Args for parameters
  %% since the problem is that high order functions
  %% don't always expect a /1 function
  Arity = length(Vars),
  Fun = 
    case Arity of
      0 ->
        fun() -> 
          eval_expr(M, concrete, CodeServer, TraceServer, Body, Env) 
        end;
      1 ->
        fun(A) ->
          NewEnv = bind_parameters([A], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      2 ->
        fun(A, B) ->
          NewEnv = bind_parameters([A, B], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      3 ->
        fun(A, B, C) ->
          NewEnv = bind_parameters([A, B, C], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      4 ->
        fun(A, B, C, D) ->
          NewEnv = bind_parameters([A, B, C, D], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      5 ->
        fun(A, B, C, D, E) ->
          NewEnv = bind_parameters([A, B, C, D, E], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      6 ->
        fun(A, B, C, D, E, F) ->
          NewEnv = bind_parameters([A, B, C, D, E, F], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      7 ->
        fun(A, B, C, D, E, F, G) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      8 ->
        fun(A, B, C, D, E, F, G, H) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      9 ->
        fun(A, B, C, D, E, F, G, H, I) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      10 ->
        fun(A, B, C, D, E, F, G, H, I, J) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      11 ->
        fun(A, B, C, D, E, F, G, H, I, J, K) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      12 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      13 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      14 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      15 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      16 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      17 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      18 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      19 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      20 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      21 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      22 ->
        fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U, V) ->
          NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U, V], Vars, Env),
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
        end;
      _ ->
        exception('error', lambda_fun_argument_limit)
    end,
    %% Wrap the fun in a semantic object
    #semantic{value=Fun, degree=1};
  
%% c_let
eval_expr(M, concrete, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Env) ->
  %% Degree of Vars sequence
  Degree = length(Vars),
  %% Evaluate the Args
  {semantic, Args, Degree} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
%  %% DEBUG ---------------------------------
%  io:format("[c_let]: Args = ~p~n", [Args]),
%  %% ---------------------------------------
  %% Bind the variables
  NewEnv = 
    case Degree of %% problem is when we have 1 argument
      1 ->
        bind_parameters([#semantic{value=Args, degree=1}], Vars, Env);
      _ ->
        bind_parameters(Args, Vars, Env)
    end,
%  %% DEBUG -------------------------------------
%  io:format("[c_let]: NewEnv = ~p~n", [NewEnv]),
%  %% -------------------------------------------
  %% Evaluate the 'in' expression of the let definition
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
  
%% c_letrec !!!!!!UNTESTED!!!!
eval_expr(M, concrete, CodeServer, TraceServer, {c_letrec, _Anno, Defs, Body}, Env) ->
  FuncNames = 
    lists:map(fun({Func, _Def}) -> Func#c_var.name end, Defs),
  %% Forward Declaration of funs
  ForwardEnv = 
    list:foldl(
      fun(Name, E) ->
        conc_lib:add_binding(Name, forward_declaration, E)
      end,
      Env, FuncNames
    ),
  %% Declare funs
  NewEnv = 
    lists:foldl(
      fun({Func, Def}, E) ->
        Name = Func#c_var.name,
        {semantic, Def_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Def, E),
        conc_lib:add_binding(Name, #semantic{value=Def_Val, degree=1}, E)
      end,
      ForwardEnv, Defs
    ),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);

%% c_literal
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Env) ->
  #semantic{value=Val, degree=1};
  
%% c_primop
eval_expr(M, concrete, CodeServer, TraceServer, {c_primop, _Anno, Name, Args}, Env) ->
  %% Evaluate primop arguments
  Args_Val = lists:map(
    fun(Arg) ->
      {semantic, Arg_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
      #semantic{value=Arg_Val, degree=1}
    end,
    Args
  ),
  %% Determine the primop
  PrimOp = Name#c_literal.val,
  %% Unwrap the semantic arguments
  A = conc_lib:semantics_to_terms(Args_Val),
  %% Call the local function TODO needs case for each primop
  Result = 
    case PrimOp of
      'raise' ->
        [Class, Reason] = A,
        eval({named, erlang, Class}, [Reason], concrete, local, CodeServer, TraceServer, self())
    end,
  %% Wrap the result to a semantic value
  conc_lib:term_to_semantic(Result);  
  
%% c_try
eval_expr(M, concrete, CodeServer, TraceServer, {c_try, _Anno, Arg, Vars, Body, Evars, Handler}, Env) ->
  try
    Degree = length(Vars),
    %% Evaluate the Arg
    {semantic, ArgVal, Degree} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
    %% Bind the variables
    NewEnv = 
      case Degree of %% problem is when we have 1 argument
        1 ->
          bind_parameters([#semantic{value=ArgVal, degree=1}], Vars, Env);
        _ ->
          bind_parameters(ArgVal, Vars, Env)
      end,
    eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
  catch
    Class:Reason ->
      %% TODO 3rd value should be stack, but...
      %% CAUTION!! 3rd Var which is stacktrace is substituted by exception class
      Vals = [#semantic{value=Class, degree=1}, #semantic{value=Reason, degree=1}, #semantic{value=Class, degree=1}],
      ExcEnv = bind_parameters(Vals, Evars, Env),
      eval_expr(M, concrete, CodeServer, TraceServer, Handler, ExcEnv)
  end;
  
%% c_seq
eval_expr(M, concrete, CodeServer, TraceServer, {c_seq, _Anno, Arg, Body}, Env) ->
  _Arg_Val = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, Env);

%% c_tuple
eval_expr(M, concrete, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Env) ->
  %% Evaluate the Args
  Es_Val = lists:map(
    fun(E) ->
      {semantic, E_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, E, Env),
      E_Val
    end,
    Es
  ),
  %% Turn the list of the evaluated Args into a tuple
  #semantic{value=list_to_tuple(Es_Val), degree=1};
  
%% c_values
eval_expr(M, concrete, CodeServer, TraceServer, {c_values, _Anno, Es}, Env) ->
  Degree = length(Es),
  Es_Val = lists:map(
    fun(E) ->
      {semantic, E_Val, 1} = eval_expr(M, concrete, CodeServer, TraceServer, E, Env),
      #semantic{value=E_Val, degree=1}
    end,
    Es
  ),
  #semantic{value=Es_Val, degree=Degree};
  
%% c_var
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env)
  when is_tuple(Name) ->
    %% If Name is a function
    case conc_lib:get_value(Name, Env) of
      {ok, Closure} -> %% declared in letdef
        Closure;
      error ->         %% either local in module or external
        #semantic{value=Name, degree=1}
    end;
    
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env) ->
  %% If it's a variable then return its value
    {ok, Value} = conc_lib:get_value(Name, Env),
    Value.
    
%% ----------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% pattern_match(Mode, TraceServer, Pat, Val) -> Match
%%   Mode :: concrete | symbolic
%%   TraceServer :: pid()
%%   Pat :: cerl()
%%   Val :: term()
%%   Match = {true, Map} | false
%%     Map :: [{semantic_var(), semantic_value()}]
%% Pattern Match an evaluated expression Val with a pattern Pat.
%% If it succeeds, it returns {true, Map}, yielding a mapping Map.
%% If it fails, it returns false.
%%----------------------------------------------------------------------

%% AtomicLiteral pattern
pattern_match(concrete, TraceServer, {c_literal, _Anno, LitVal}, V) ->
  case LitVal =:= V of
    true ->
      send_trace(TraceServer, {match_success, LitVal, V}),
      {true, []};
    false ->
      send_trace(TraceServer, {match_fail, LitVal, V}),
      false
  end;
  
%% VariableName pattern
pattern_match(concrete, TraceServer, {c_var, _Anno, Name}, V) ->
  send_trace(TraceServer, {match_success, Name, V}),
  SemanticV = conc_lib:term_to_semantic(V),
  {true, [{Name, SemanticV}]};
  
%% Tuple pattern
pattern_match(concrete, TraceServer, {c_tuple, _Anno, Es}, V)
  when is_tuple(V) ->
    Vs = tuple_to_list(V),
    pattern_match_all(concrete, TraceServer, Es, Vs);
  
pattern_match(concrete, TraceServer, {c_tuple, _Anno, _Es}, V) ->
  send_trace(TraceServer, {match_fail, val_not_tuple, V}),
  false;
  
%% List constructor pattern
pattern_match(concrete, TraceServer, {c_cons, _Anno, Hd, Tl}, [V|Vs]) ->
  case pattern_match(concrete, TraceServer, Hd, V) of
    false ->
      false;
    {true, Mapping_Hd} ->
      case pattern_match(concrete, TraceServer, Tl, Vs) of
        false ->
          false;
        {true, Mapping_Tl} ->
          {true, Mapping_Hd ++ Mapping_Tl}
      end
  end;
  
pattern_match(concrete, TraceServer, {c_cons, _Anno, _Hd, _Tl}, V) ->
  send_trace(TraceServer, {match_fail, val_not_list, V}),
  false;
  
%% Alias pattern
pattern_match(concrete, TraceServer, {c_alias, _Anno, Var, Pat}, V) ->
  Match = pattern_match(concrete, TraceServer, Pat, V),
  case Match of
    false ->
      false;
    {true, Mapping} ->
      VarName = Var#c_var.name,
      SemanticV = conc_lib:term_to_semantic(V),
      {true, [{VarName, SemanticV}|Mapping]}
  end.

%% Helper functions pattern_match_all/4 and pattern_match_all/5
%% that apply pattern_matching to a sequence of patterns and values
pattern_match_all(concrete, TraceServer, Pats, EVals) ->
  pattern_match_all(concrete, TraceServer, Pats, EVals, []).
    
pattern_match_all(concrete, _TraceServer, [] ,[], Mappings) ->
  {true, Mappings};
pattern_match_all(concrete, _TraceServer, _Pats, [], _Mappings) ->
  false;
pattern_match_all(concrete, _TraceServer, [], _EVals, _Mappings) ->
  false;
pattern_match_all(concrete, TraceServer, [Pat|Pats], [EVal|EVals], Mappings) ->
%  %% DEBUG ---------------------------------------------------------------------
%  io:format("[pat_match_all]: Will try to match ~n~p~n with ~p~n", [Pat, EVal]),
%  %% ---------------------------------------------------------------------------
  Match = pattern_match(concrete, TraceServer, Pat, EVal),
  case Match of
    {true, Maps} ->
      pattern_match_all(concrete, TraceServer, Pats, EVals, Maps ++ Mappings);
    false ->
      false
  end.

%%----------------------------------------------------------------------------
%% find_clause(M, Mode, CodeServer, TraceServer, Cls, Val, Env) -> Match
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Cls :: [#c_clause{}]
%%   Val :: #semantic{}
%%   Env :: conc_lib:environment()
%%   Match = {Body, NewEnv}
%%     Body :: cerl()
%%     NewEnv :: conc_lib:environment()
%% Matches an evaluated switch expression Val with the appropriate
%% clause from the list Cls and returns the Body that will be
%% evaluated next and the new environment (that includes the mappings)
%%----------------------------------------------------------------------------
find_clause(M, concrete, CodeServer, TraceServer, Clauses, Val, Env) ->
  find_clause(M, concrete, CodeServer, TraceServer, Clauses, Val, Env, 1).
  
%% Helper function find_clause/8
find_clause(_M, concrete, _CodeServer, TraceServer, [], Val, _Env, _Cnt) ->
  send_trace(TraceServer, {no_match_clause, Val#semantic.value}),
  %% TODO fix exception
  exception(error, no_match_clause);
find_clause(M, concrete, CodeServer, TraceServer, [Cl|Cls], Val, Env, Cnt) ->
  Match = match_clause(M, concrete, CodeServer, TraceServer, Cl, Val, Env, Cnt),
  case Match of
    false ->
      find_clause(M, concrete, CodeServer, TraceServer, Cls, Val, Env, Cnt+1);
    {true, Body, NewEnv} ->
      {Body, NewEnv}
  end.
  
%%----------------------------------------------------------------------------
%% match_clause(M, Mode, CodeServer, TraceServer, Cl, Val, Env, Cnt) -> Match
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Cl  :: #c_clause{}
%%   Val :: #semantic{}
%%   Env :: conc_lib:environment()
%%   Cnt :: non_neg_integer()
%%   Match = {true, Body, NewEnv} | false
%%     Body :: cerl()
%%     NewEnv :: conc_lib:environment()
%% Matches a clause Cl with an evaluated switch expression Val.
%% If the match succeeds, it returns the Body that will be evaluated and
%% the new environment (that includes the added mappings).
%% If the match fails, it returns false.
%%----------------------------------------------------------------------------
match_clause(M, concrete, CodeServer, TraceServer, {c_clause, _Anno, Pats, Guard, Body}, ArgVal, Env, Cnt)
  when length(Pats) =:= ArgVal#semantic.degree ->
    send_trace(TraceServer, {clause_try, Cnt}),
    EVals = 
      case ArgVal#semantic.degree of
        1 -> [conc_lib:semantic_to_term(ArgVal)];
        _ -> conc_lib:semantics_to_terms(ArgVal#semantic.value)
      end,
    Match = pattern_match_all(concrete, TraceServer, Pats, EVals),
    case Match of
      false ->
        false;
      {true, Ms} ->
        NewEnv = conc_lib:add_mappings_to_environment(Ms, Env),
        {semantic, Res, 1} = eval_expr(M, concrete, CodeServer, TraceServer, Guard, NewEnv),
        case Res of
          false ->
            send_trace(TraceServer, {guard_fail, Guard}),
            false;
          true ->
            send_trace(TraceServer, {guard_success, Guard}),
            send_trace(TraceServer, {clause_match, Cnt}),
            {true, Body, NewEnv}
        end
    end;

match_clause(_M, concrete, _CodeServer, TraceServer, {c_clause, _Anno, _Pats, _Guard, _Body}, _ArgVal, _Env, Cnt) ->
  send_trace(TraceServer, {clause_try, Cnt}),
  send_trace(TraceServer, {wrong_number_of_pats}),
  send_trace(TraceServer, {clause_fail, Cnt}),
  false.


%%====================================================================
%% Helper functions
%%====================================================================

find_call_type(M1, M2) ->
  case M1 =:= M2 of
    true  -> local;
    false -> external
  end.

will_spawn_process(erlang, spawn, 1) -> true;
will_spawn_process(_Mod, _Fun, _Arity) -> false.

