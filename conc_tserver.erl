-module(conc_tserver).
-behaviour(gen_server).

%% gen_server callbacks
 -export([init/1, terminate/2, handle_call/3,
  code_change/3, handle_info/2, handle_cast/2]).

%-compile([export_all]).

%% gen_server state datatype
-record(state, {
  super,
  procs,       %% List of Pids of Live Evaluator processes
  traces,
  ptree
}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Super]) ->
  process_flag(trap_exit, true),
  Traces = ets:new(?MODULE, [ordered_set, protected]),
  Ptree = ets:new(?MODULE, [bag, protected]),
  {ok, #state{super=Super, procs=[], traces=Traces, ptree=Ptree}}.
  
terminate(_Reason, State) ->
  Super = State#state.super,
  Traces = State#state.traces,
  Ptree = State#state.ptree,
  %% TODO
  %% reconstruct Process Tree and Traces Tree
  %%
  ets:delete(Traces),
  ets:delete(Ptree),
  Super ! {self(), State},
  ok.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.
  
handle_cast(_Msg, State) ->
  {noreply, State}.

  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_server]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.

