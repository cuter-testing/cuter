-module(conc_tserver).
-behaviour(gen_server).

%% External exports
-export([init_traceserver/0]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
  code_change/3, handle_info/2, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  super,    %% Supervisor process :: pid()
  procs,    %% List of Pids of Live Evaluator processes :: [pid()]
  traces,   %% ETS table where traces are stored :: ets:tid()
  ptree,    %% ETS table where {Parent, Child} process pids are stored :: {pid(), pid()}
  links,    %% ETS table where {Pid, Linked}
  logs      %% Proplist to store log informations // currently only {procs, NumOfMonitoredProcs}
}).

%%====================================================================
%% External exports
%%====================================================================

init_traceserver() ->
  {ok, TraceServer} = gen_server:start_link(?MODULE, [self()], []),
  TraceServer.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Super]) ->
  process_flag(trap_exit, true),
  Traces = ets:new(?MODULE, [ordered_set, protected]),
  Ptree = ets:new(?MODULE, [bag, protected]),
  Links = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #state{super=Super, procs=[], traces=Traces, ptree=Ptree, links=Links, logs=[{procs, 0}]}}.
  
terminate(_Reason, State) ->
  Super = State#state.super,
  Traces = State#state.traces,
  Ptree = State#state.ptree,
  Links = State#state.links,
  Logs = State#state.logs,
  %% TODO
  %% reconstruct Process Tree and Traces Tree
  %%
%    io:format("***** Traces *****~n"),
%    report(Super, Traces, Ptree),
%    io:format("******************~n"),
  %%
  %%
  ets:delete(Traces),
  ets:delete(Ptree),
  ets:delete(Links),
  %% Send Logs to supervisor
  Super ! {self(), Logs},
  process_flag(trap_exit, false),
  ok.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_call({register_parent, Parent, Link}, {From, _FromTag}, State) ->
  Procs = State#state.procs,
  Ptree = State#state.ptree,
  Links = State#state.links,
  Logs = State#state.logs,
  FromPid = 
    case is_atom(From) of
     true ->  whereis(From);
     false -> From
    end,
  monitor(process, FromPid),
  io:format("[conc_tserver]: Monitoring ~p~n", [FromPid]),
  ets:insert(Links, {FromPid, Link}),
  ets:insert(Ptree, {Parent, FromPid}),
  NewProcs = [FromPid|Procs],
  {procs, P} = lists:keyfind(procs, 1, Logs),
  NewLogs = lists:keyreplace(procs, 1, Logs, {procs, P+1}),
  {reply, ok, State#state{procs=NewProcs, logs=NewLogs}};
  
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.
  
handle_cast({trace, Who, Trace}, State) ->
  Traces = State#state.traces,
  EncTrace = term_to_binary(Trace),
  case ets:lookup(Traces, Who) of
    [] ->
      ets:insert(Traces, {Who, [EncTrace]});
    [{Who, OldTrace}] ->
      ets:insert(Traces, {Who, [EncTrace | OldTrace]})
  end,
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Who, normal}, State) ->
  Procs = State#state.procs,
  NewProcs = lists:delete(Who, Procs),
  case NewProcs of
    [] ->
      {stop, normal, State#state{procs=NewProcs}};
    _  ->
      {noreply, State#state{procs=NewProcs}}
  end;
  
handle_info({'DOWN', _Ref, process, Who, Reason}, State) ->
  Super = State#state.super,
  Procs = State#state.procs,
  Links = State#state.links,
  NewProcs = lists:delete(Who, Procs),
  [{Who, Link}] = ets:lookup(Links, Who),
  case Link of
    %% If process is linked/monitored to another then do nothing
    true ->
      ets:delete(Links, Who),
      {noreply, State#state{procs=NewProcs}};
    %% If process is not linked/monitored to another
    %% then report the exception and stop execution
    false ->
%      io:format("[TraceServer]: Exception in ~p  : ~p~n", [Who, Reason]),
%      io:format("Killings procs ~p~n", [NewProcs]),
      %% Killing all remaining alive processes
      kill_all_processes(NewProcs),
      %% Send Msg to Super
      Super ! {error, {Who, Reason}},
      {stop, normal, State#state{procs=[]}}
  end;
  
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_tserver]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.
  
%%====================================================================
%% Internal functions
%%====================================================================

%report([], _Traces, _Ptree) ->
%  ok;
%  
%report(Super, Traces, Ptree) 
%  when is_pid(Super) ->
%    io:format("[conc_tserver]: Skipping Super ~p~n", [Super]),
%    L = ets:lookup(Ptree, Super),
%    Procs = lists:map(fun({_X, Y}) -> Y end, L),
%    report(Procs, Traces, Ptree);
%    
%report([Proc|Procs], Traces, Ptree) ->
%  [{Proc, Trace}] = ets:lookup(Traces, Proc),
%  io:format("[conc_tserver]: Trace of ~p:~n~p~n", [Proc, lists:reverse(Trace)]),
%  case ets:lookup(Ptree, Proc) of
%    [] ->
%      report(Procs, Traces, Ptree);
%    L ->
%      NewProcs = lists:map(fun({_X, Y}) -> Y end, L),
%      report(Procs ++ NewProcs, Traces, Ptree)
%  end.
  
kill_all_processes(Procs) ->
  KillOne = fun(P) ->
    case is_process_alive(P) of
      true ->
        exit(P, kill);
      false ->
        ok
    end
  end,
  lists:map(KillOne, Procs).
