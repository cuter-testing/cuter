%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_eval).

-include("cuter_macros.hrl").

-compile(export_all).
-export_type([result/0, valuelist/0]).


-type result()   :: {any(), any()}.
%% Used to represent list of values for Core Erlang interpretation
-record(valuelist, {
  values :: [any()],
  degree :: non_neg_integer()
}).
-type valuelist() :: #valuelist{}.


%% -------------------------------------------------------------------
%% Wrapper exported function that spawns an interpreter process 
%% which returns the value of an MFA call to the interpreter server
%% -------------------------------------------------------------------
-spec i(module(), atom(), [any()], servers()) -> pid().
i(M, F, As, Servers) ->
  Root = self(),
  I = 
    fun() ->
      {SymbAs, Mapping} = cuter_symbolic:abstract(As),
      {ok, Fd} = cuter_monitor:subscribe(Servers#svs.monitor, Root),
%      concolic_encdec:log(Fd, 'params', SymbAs),
%      log_mfa_spec(Fd, {M, F, length(As)}, SymbAs, CodeServer),
      cuter_iserver:send_mapping(Root, Mapping),
      NMF = {named, M, F},
%      Val = eval(NMF, As, SymbAs, external, CodeServer, TraceServer, Fd),
      Ret = apply(M, F, As),
      cuter_iserver:int_return(Root, Ret)
    end,
  erlang:spawn(I).

%% -------------------------------------------------------------------
%% eval
%%
%% Concrete/Symbolic Evaluation and Logging of an MFA call
%% -------------------------------------------------------------------

