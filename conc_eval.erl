-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").


%% Concrete Evaluation of MFA
%% M, F, A, Mode, CallType, Parent, CodeServer, TraceServer, Register
eval(M, F, A, concrete, CallType, Parent, CodeServer, TraceServer, Register) ->
  register_to_trace(Register, TraceServer, Parent),
  send_trace(TraceServer, {func_in, M, F, A}), %% Trace
  %% TODO Some kind of caching instead of constantly querying CodeServer
  case get_module_db(M, CodeServer) of
    unloadable ->
      apply(M, F, A);
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
          Result = eval_core(M, concrete, CodeServer, TraceServer, Def#c_fun.body, Environment),
          send_trace(TraceServer, {func_out, M, F, A, Result}) %% Trace
      end
  end.
  
%% Register self() and Parent to the Trace Server
register_to_trace(true, TraceServer, Parent) ->
  gen_server:call(TraceServer, {register_parent, Parent});
  
register_to_trace(false, _TraceServer, _Parent) ->
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
%% exception(Class, Reason, Env)
%%   Class = error | exit | throw
%%   Reason = term()
%%   Env = conc_eval_bindings:environment()
%% Get the information about the error and then raise the exception.
%%--------------------------------------------------------------------
exception(Class, Reason) ->
  erlang:Class(Reason).

%% Evaluate Core Erlang

%% c_apply
eval_core(M, concrete, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Env) ->
  Eval_Args = lists:map(
    fun(Arg) ->
      eval_core(M, concrete, CodeServer, TraceServer, Arg, Env)
    end,
    Args
  ),
%  io:format("[c_apply]: Eval_Args = ~p~n", [Eval_Args]),
  case eval_core(M, concrete, CodeServer, TraceServer, Op, Env) of
    {Fun, _Arity} -> 
      %% TODO use eval and not apply
      apply(M, Fun, Eval_Args);
    Closure ->
      apply(Closure, [Eval_Args])
  end;

%% c_cons
eval_core(M, concrete, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Env) ->
  Eval_Hd = eval_core(M, concrete, CodeServer, TraceServer, Hd, Env),
  Eval_Tl = eval_core(M, concrete, CodeServer, TraceServer, Tl, Env),
  [Eval_Hd | Eval_Tl];
  
%% c_fun
eval_core(M, concrete, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Env) ->
  fun(Args) ->
    NewEnv = bind_parameters(Args, Vars, Env),
%    io:format("[c_fun]: NewEnv = ~p~n", [NewEnv]),
    eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv)
  end;
  
%% c_let
eval_core(M, concrete, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Env) ->
  Args = eval_core(M, concrete, CodeServer, TraceServer, Arg, Env),
%  io:format("[c_let]: Args = ~p~n", [Args]),
  case is_list(Args) of
    true ->
      NewEnv = bind_parameters(Args, Vars, Env);
    false ->
      NewEnv = bind_parameters([Args], Vars, Env)
  end,
%  io:format("[c_let]: NewEnv = ~p~n", [NewEnv]),
  eval_core(M, concrete, CodeServer, TraceServer, Body, NewEnv);

%% c_literal
eval_core(_M, concrete, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Env) ->
  Val;

%% c_tuple
eval_core(M, concrete, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Env) ->
  Eval_Es = lists:map(
    fun(E) ->
      eval_core(M, concrete, CodeServer, TraceServer, E, Env)
    end,
    Es
  ),
  list_to_tuple(Eval_Es);
  
%% c_var
eval_core(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env) ->
  case Name of 
    {_Fun, _Arity} ->
      Name;
    _ ->
      {ok, Value} = conc_eval_bindings:get_value(Name, Env),
      Value
  end.
