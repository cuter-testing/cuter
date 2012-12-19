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
          Parent ! error_not_exported;
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
  init_fun_parameters(Args, Vars_c, Env).
  
init_fun_parameters([], [], Env) ->
  Env;
init_fun_parameters([Arg|Args], [Var_c|Vars_c], Env) ->
  {ok, NewEnv} = conc_eval_bindings:add_binding(Var_c#c_var.name, Arg, Env),
  init_fun_parameters(Args, Vars_c, NewEnv).
  
%% Send Trace Data to Trace Server
send_trace(TraceServer, Msg) ->
  gen_server:cast(TraceServer, {trace, self(), Msg}).

%% Evaluate Core Erlang
eval_core(_M, concrete, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Env) ->
  Val;

eval_core(M, concrete, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Env) ->
  Eval_Es = lists:map(
    fun(E) ->
      eval_core(M, concrete, CodeServer, TraceServer, E, Env)
    end,
    Es
  ),
  list_to_tuple(Eval_Es).
