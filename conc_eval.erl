-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").


%% Concrete Evaluation of MFA
%% M, F, A, Mode, Trace, CodeServer, CallType
eval(M, F, A, concrete, CallType, Parent, CodeServer, TraceServer, Register) ->
  register_parent(Register, TraceServer, Parent),
  %% TODO Some kind of caching instead of constantly querying CodeServer
  case get_module_db(M, CodeServer) of
    unloadable ->
      apply(M, F, A);
    {ok, ModDb} ->
    
      %% Get function info
      Arity = length(A),
      Key = {M, F, Arity},
      [{Key, {_Def, Exported}}] = ets:lookup(ModDb, Key),
      
      %% Check if CallType is compatible
      case check_exported(Exported, CallType) of
        false ->
          %% TODO Should raise exception
          error;
        true ->
          Parent ! ok
      end
  end.
  
register_parent(true, TraceServer, Parent) ->
  gen_server:call(TraceServer, {register_parent, Parent});
  
register_parent(false, _TraceServer, _Parent) ->
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
  

