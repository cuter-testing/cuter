-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").



%%--------------------------------------------------------------------------
%% eval(M, F, A, Mode, CallType, CodeServer, TraceServer, Register)
%%   M :: atom()
%%   F :: atom()
%%   A :: [term()]
%%   Mode :: concrete | symbolic
%%   CallType :: local | external
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Register :: {true, Parent} | false
%%    Parent :: pid()
%% Interpret and trace Core Erlang ASTs (concretely or symbolically)
%%--------------------------------------------------------------------------

%% Concrete Evaluation of MFA
eval(M, F, A, concrete, CallType, CodeServer, TraceServer, Register) ->
  register_to_trace(Register, TraceServer),
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
%      io:format("!!~n~p~n!!~n",[ets:lookup(ModDb, Key)]),
      [{Key, {Def, Exported}}] = ets:lookup(ModDb, Key),
      
      %% Check if CallType is compatible
      case check_exported(Exported, CallType) of
        false ->
          %% Calling function that is not exported
          %% from outside the module
          %% TODO Should raise exception
          ErrorMsg = lists:flatten(io_lib:format("Not Exported function ~p:~p/~p", [M, F, Arity])),
          exception('error', ErrorMsg);
        true ->
          %% Bind function's parameters to the Arguments
%          io:format("Def: ~p~n", [Def]),
          Environment = init_fun_parameters(A, Def#c_fun.vars),
%          Result = Def,
          Result = eval_core(M, concrete, CodeServer, TraceServer, Def#c_fun.body, Environment),
          send_trace(TraceServer, {func_out, M, F, A, Result}), %% Trace
          Result
      end
  end.
  
%% Register self() and Parent to the Trace Server
register_to_trace({true, Parent}, TraceServer) ->
  gen_server:call(TraceServer, {register_parent, Parent});
  
register_to_trace(false, _TraceServer) ->
  ok.

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

check_exported(true, _CallType) ->
  true;
check_exported(false, CallType) ->
  case CallType of
    local ->
      true;
    external ->
      false
  end.
  
%% Creates a new Environment where function's paremeters
%% are bound to the actual arguments
init_fun_parameters(Args, Vars_c) ->
  Env = conc_eval_bindings:new_environment(),
  bind_parameters(Args, Vars_c, Env).
  
bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var_c|Vars_c], Env) ->
  {ok, NewEnv} = conc_eval_bindings:add_binding(Var_c#c_var.name, Arg, Env),
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
%% eval_core(M, Mode, CodeServer, TraceServer, Expr, Env)
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Expr :: cerl()
%%   Env :: conc_eval_bindings:environment()
%% Evaluate Core Erlang ASTs in record format
%%--------------------------------------------------------------------

%% c_apply
eval_core(M, concrete, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Env) ->
  %% Evaluate Op
  Eval_Op = eval_core(M, concrete, CodeServer, TraceServer, Op, Env),
  %% Evaluate Args
  Eval_Args = lists:map(
    fun(Arg) ->
      eval_core(M, concrete, CodeServer, TraceServer, Arg, Env)
    end,
    Args
  ),
%  %% DEBUG ---------------------------------------------
%  io:format("[c_apply]: Eval_Op = ~p~n", [Eval_Op]),
%  io:format("[c_apply]: Eval_Args = ~p~n", [Eval_Args]),
%  %% ---------------------------------------------------
  case Eval_Op of
    {Fun, _Arity} -> 
      %% Call the local function
      eval(M, Fun, Eval_Args, concrete, local, CodeServer, TraceServer, false);
    Closure ->
      %% Apply the evaluated Args to the closure
      apply(Closure, Eval_Args)
  end;
  
%% c_call
eval_core(M, concrete, CodeServer, TraceServer, {c_call, _Anno, Mod, Name, Args}, Env) ->
  %% Evaluate Module name
  Eval_Mod = eval_core(M, concrete, CodeServer, TraceServer, Mod, Env),
  %5 Evaluate Function name
  Eval_Name = eval_core(M, concrete, CodeServer, TraceServer, Name, Env),
  %% Evaluate Args
  Eval_Args = lists:map(
    fun(Arg) ->
      eval_core(M, concrete, CodeServer, TraceServer, Arg, Env)
    end,
    Args
  ),
  %% Call MFA
  case M =:= Eval_Mod of
    true ->
      eval(Eval_Mod, Eval_Name, Eval_Args, concrete, local, CodeServer, TraceServer, false);
    false ->
      eval(Eval_Mod, Eval_Name, Eval_Args, concrete, external, CodeServer, TraceServer, false)
  end;

%% c_cons
eval_core(M, concrete, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Env) ->
  %% Evaluate the head of the list
  Eval_Hd = eval_core(M, concrete, CodeServer, TraceServer, Hd, Env),
  %% Evaluate the tail of the list
  Eval_Tl = eval_core(M, concrete, CodeServer, TraceServer, Tl, Env),
  %% Apply the list constructor
  [Eval_Hd | Eval_Tl];
  
%% c_fun
eval_core(M, concrete, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Env) ->
  %% Manually creating anonymous func
  %% and not use a list Args for parameters
  %% since the problem is that high order functions
  %% don't always expect a /1 function
  Arity = length(Vars),
  case Arity of
    0 ->
      fun() -> 
        eval_core(M, concrete, CodeServer, TraceServer, Body, Env) 
      end;
    1 ->
      fun(A) ->
        NewEnv = bind_parameters([A], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    2 ->
      fun(A, B) ->
        NewEnv = bind_parameters([A, B], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    3 ->
      fun(A, B, C) ->
        NewEnv = bind_parameters([A, B, C], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    4 ->
      fun(A, B, C, D) ->
        NewEnv = bind_parameters([A, B, C, D], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        NewEnv = bind_parameters([A, B, C, D, E], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        NewEnv = bind_parameters([A, B, C, D, E, F], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    11 ->
      fun(A, B, C, D, E, F, G, H, I, J, K) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    12 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    13 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    14 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    15 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    16 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    17 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    18 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    19 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    20 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    21 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    22 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U, V) ->
        NewEnv = bind_parameters([A, B, C, D, E, F, G, H, I, J, K, L, Z, N, O, P, Q, R, S, T, U, V], Vars, Env),
        eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
      end;
    _ ->
      exception('error', lambda_fun_argument_limit)
  end;
  
%% c_let
eval_core(M, concrete, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Env) ->
  %% Evaluate the Args
  Args = eval_core(M, concrete, CodeServer, TraceServer, Arg, Env),
%  %% DEBUG ---------------------------------
%  io:format("[c_let]: Args = ~p~n", [Args]),
%  %% ---------------------------------------
  %% Bind the variables
  case length(Vars) =:= 1 of %% problem is when we have 1 argument
    true ->
      NewEnv = bind_parameters([Args], Vars, Env);
    false ->
      NewEnv = bind_parameters(Args, Vars, Env)
  end,
%  %% DEBUG -------------------------------------
%  io:format("[c_let]: NewEnv = ~p~n", [NewEnv]),
%  %% -------------------------------------------
  %% Evaluate the 'in' expression of the let definition
  eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv);

%% c_literal
eval_core(_M, concrete, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Env) ->
  Val;

%% c_tuple
eval_core(M, concrete, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Env) ->
  %% Evaluate the Args
  Eval_Es = lists:map(
    fun(E) ->
      eval_core(M, concrete, CodeServer, TraceServer, E, Env)
    end,
    Es
  ),
  %% Turn the list of the evaluated Args into a tuple
  list_to_tuple(Eval_Es);
  
%% c_var
eval_core(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env) ->
  case Name of
    %% If it's a function then return {Function, Arity}
    {_Fun, _Arity} ->
      Name;
    _ ->
    %% If it's a variable then return its value
      {ok, Value} = conc_eval_bindings:get_value(Name, Env),
      Value
  end.
