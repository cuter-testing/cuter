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
  case gen_server:start_link(?MODULE, [self()], []) of
    {ok, TraceServer} -> TraceServer;
    {error, Reason}   -> erlang:error({traceserver_init, Reason})
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Super]) ->
  process_flag(trap_exit, true),
  Traces = ets:new(?MODULE, [ordered_set, protected]),
  Ptree = ets:new(?MODULE, [bag, protected]),
  Links = ets:new(?MODULE, [ordered_set, protected]),
  Procs = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #state{super=Super, procs=Procs, traces=Traces, ptree=Ptree, links=Links, logs=[{procs, 0}]}}.
  
terminate(_Reason, State) ->
  Super = State#state.super,
  Traces = State#state.traces,
  Procs =  State#state.procs,
  Ptree = State#state.ptree,
  Links = State#state.links,
  Logs = State#state.logs,
  %% TODO
  %% reconstruct Process Tree and Traces Tree
  %%
  ets:delete(Traces),
  ets:delete(Ptree),
  ets:delete(Procs),
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
%  io:format("[conc_tserver]: Monitoring ~p~n", [FromPid]),
  ets:insert(Links, {FromPid, Link}),
  ets:insert(Ptree, {Parent, FromPid}),
  ets:insert(Procs, {FromPid, true}),
  {procs, P} = lists:keyfind(procs, 1, Logs),
  NewLogs = lists:keyreplace(procs, 1, Logs, {procs, P+1}),
  {reply, ok, State#state{logs=NewLogs}};
  
handle_call({is_monitored, Who}, {_From, _FromTag}, State) ->
  Procs = State#state.procs,
  WhoPid = 
    case is_atom(Who) of
     true ->  whereis(Who);
     false -> Who
    end,
  Monitored = 
    case ets:lookup(Procs, WhoPid) of
      [] -> false;
      [{WhoPid, true}] -> true
    end,
  {reply, Monitored, State};
  
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
  ets:delete(Procs, Who),
  case ets:first(Procs) of
    '$end_of_table' ->
      {stop, normal, State};
    _  ->
      {noreply, State}
  end;
  
handle_info({'DOWN', _Ref, process, Who, Reason}, State) ->
%  io:format("Got error from ~w => ~w~n",[Who,Reason]),
  Super = State#state.super,
  Procs = State#state.procs,
  Links = State#state.links,
  ets:delete(Procs, Who),
  [{Who, Link}] = ets:lookup(Links, Who),
  case Link of
    %% If process is linked/monitored to another then do nothing
    true ->
      case ets:first(Procs) of 
        '$end_of_table' ->
          {stop, normal, State};
        _ ->
          ets:delete(Links, Who),
          {noreply, State}
      end;
    %% If process is not linked/monitored to another
    %% then report the exception and stop execution
    false ->
%      io:format("[TraceServer]: Exception in ~p  : ~p~n", [Who, Reason]),
%      io:format("Killings procs ~p~n", [NewProcs]),
      %% Killing all remaining alive processes
      kill_all_processes(ets:tab2list(Procs)),
      %% Send Msg to Super
      Super ! {error, {Who, Reason}},
      {stop, normal, State}
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
      true  -> exit(P, kill);
      false -> ok
    end
  end,
  lists:map(KillOne, Procs).
