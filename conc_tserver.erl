-module(conc_tserver).
-behaviour(gen_server).

%% gen_server callbacks
 -export([init/1, terminate/2, handle_call/3
% , code_change/3, handle_info/2,
%   handle_cast/2]).
]).
%-compile([export_all]).

%% gen_server state datatype
-record(state, {
  super,
  evals,       %% List of Pids of Live Evaluator processes
  traces,
  ptree
}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Super]) ->
  Traces = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #state{super=Super, evals=[], traces=Traces, ptree=[]}}.
  
terminate(_Reason, State) ->
  Traces = State#state.traces,
  Super = State#state.super,
  ets:delete(Traces),
  Super ! {self(), State},
  ok.
  
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.


