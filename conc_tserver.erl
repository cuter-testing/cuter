-module(conc_tserver).
-behaviour(gen_server).

%% External exports
-export([init_traceserver/2, terminate/1, register_to_trace/2,
  is_monitored/2, node_servers/2, file_descriptor/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
  code_change/3, handle_info/2, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  super :: pid(),                     %% Concolic Server (supervisor) process
  procs :: ets:tab(),                 %% Pids of Live Evaluator processes
  ptree :: ets:tab(),                 %% ETS table where {Parent, Child} process pids are stored
  fds   :: ets:tab(),                 %% ETS table where {Pid, Fd} are stored
  dir   :: string(),                  %% Directory where traces are saved
  logs  :: [proplists:property()]     %% Proplist to store log informations // currently only {procs, NumOfMonitoredProcs}
}).

%%====================================================================
%% External exports
%%====================================================================

%% Initialize a TraceServer
init_traceserver(TraceDir, Super) ->
  case gen_server:start(?MODULE, [TraceDir, Super], []) of
    {ok, TraceServer} -> TraceServer;
    {error, Reason}   -> exit({traceserver_init, Reason})
  end.
  
%% Terminate a TraceServer
terminate(TraceServer) ->
  gen_server:cast(TraceServer, {terminate, self()}).
  
%% Register a new process so that the TraceServer can monitor it
register_to_trace(TraceServer, Parent) ->
  {ok, Filename} = gen_server:call(TraceServer, {register_parent, Parent}),
  {ok, File} = encdec:create_file(Filename),
  store_file_descriptor(TraceServer, File),
  ok = encdec:log_term(File, {pid, self()}),
%  ok = encdec:close_file(File),
  {ok, File}.
  
%% Check if a process is monitored by TraceServer
is_monitored(TraceServer, Who) ->
  gen_server:call(TraceServer, {is_monitored, Who}).
  
%% Request the CodeServer and TraceServer of a specific node
node_servers(TraceServer, Node) ->
  {ok, Servers} = gen_server:call(TraceServer, {node_servers, Node}),
  Servers.
  
%% Request the file descriptor of a this process's trace file
file_descriptor(TraceServer) ->
  {ok, Fd} = gen_server:call(TraceServer, {get_fd, self()}),
  Fd.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Dir, Super]) ->
  process_flag(trap_exit, true),
  link(Super),
  Ptree = ets:new(?MODULE, [bag, protected]),
  Fds = ets:new(?MODULE, [ordered_set, protected]),
  Procs = ets:new(?MODULE, [ordered_set, protected]),
  U = erlang:ref_to_list(erlang:make_ref()),
  TraceDir = filename:absname(Dir ++ "/trace-" ++ U),
  ok = filelib:ensure_dir(TraceDir ++ "/"),  %% Create the directory
  InitState = #state{
    super = Super,
    procs = Procs,
    ptree = Ptree,
    fds = Fds,
    dir = TraceDir,
    logs = [{procs, 0}, {dir, TraceDir}]
  },
  {ok, InitState}.
  
terminate(_Reason, State) ->
  Super = State#state.super,
  Procs =  State#state.procs,
  Ptree = State#state.ptree,
  Fds = State#state.fds,
  Logs = State#state.logs,
  %% TODO
  %% reconstruct Process Tree and Traces Tree
  %%
  ets:delete(Ptree),
  ets:delete(Procs),
  ets:delete(Fds),
  %% Send Logs to supervisor
  ok = conc:send_tlogs(Super, Logs),
  ok.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
%% Call Request : {register_parent, Parent, Link}
%% Ret Msg : ok
handle_call({register_parent, Parent}, {From, _FromTag}, State) ->
  Procs = State#state.procs,
  Ptree = State#state.ptree,
  Dir = State#state.dir,
  Logs = State#state.logs,
  FromPid = 
    case is_atom(From) of
     true ->  whereis(From);
     false -> From
    end,
  monitor(process, FromPid),
%  io:format("[conc_tserver(~p)]: Monitoring ~p~n", [node(), FromPid]),
  ets:insert(Ptree, {Parent, FromPid}),
  ets:insert(Procs, {FromPid, true}),
  %% Update Logs - Number of monitored processes
  {procs, P} = lists:keyfind(procs, 1, Logs),
  NewLogs = lists:keyreplace(procs, 1, Logs, {procs, P+1}),
  F = erlang:pid_to_list(FromPid),
  Filename = filename:absname(Dir ++ "/proc-" ++ F),
  {reply, {ok, Filename}, State#state{logs=NewLogs}};
  
%% Call Request : {is_monitored, Who}
%% Ret Msg : boolean()
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
  
%% Call Request : {node_servers, Node}
%% Ret Msg : {ok, {CodeServer, TraceServer}}
handle_call({node_servers, Node}, _From, State) ->
  Super = State#state.super,
  Servers = conc:node_servers(Super, Node),
  {reply, {ok, Servers}, State};
  
%% Call Request : {get_fd, Who}
%% Ret Msg : {ok, FileDescriptor}
handle_call({get_fd, Who}, _From, State) ->
  Fds = State#state.fds,
  [{Who, Fd}] = ets:lookup(Fds, Who),
  {reply, {ok, Fd}, State}.

%% Cast Request : {store_fd, FromWho}
handle_cast({store_fd, From, Fd}, State) ->
  Fds = State#state.fds,
  ets:insert(Fds, {From, Fd}),
  {noreply, State};
  
%% Cast Request : {terminate, FromWho}
handle_cast({terminate, FromWho}, State) ->
  Super = State#state.super,
  Procs = State#state.procs,
  case FromWho =:= Super of
    true ->
      kill_all_processes(get_procs(Procs)),
      {stop, normal, State};
    false ->
      {noreply, State}
  end.

%% Msg when a monitored process exited normally
handle_info({'DOWN', _Ref, process, Who, normal}, State) ->
  Procs = State#state.procs,
  Fds = State#state.fds,
  ets:delete(Fds, Who),
  ets:delete(Procs, Who),
  case ets:first(Procs) of
    '$end_of_table' ->
      {stop, normal, State};
    _  ->
      {noreply, State}
  end;
  
%% Msg when a monitored process experienced an exception
handle_info({'DOWN', _Ref, process, Who, Reason}, State) ->
  Super = State#state.super,
  Procs = State#state.procs,
  Fds = State#state.fds,
  ets:delete(Procs, Who),
  ets:delete(Fds, Who),
  %% Killing all remaining alive processes
  kill_all_processes(get_procs(Procs)),
  %% Send the Error Report to the supervisor
  conc:send_error_report(Super, Who, conc_eval:unzip_error(Reason)),
  {stop, normal, State};

handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_tserver(~p)]: Unexpected message ~p~n", [node(),Msg]),
  {noreply, State}.
  
%%====================================================================
%% Internal functions
%%====================================================================

%% Stores the file descriptor of a process's trace
store_file_descriptor(TraceServer, Fd) ->
  gen_server:cast(TraceServer, {store_fd, self(), Fd}).

%% Kill all monitored processes
-spec kill_all_processes(Procs) -> ok
  when Procs :: [pid()].
  
kill_all_processes(Procs) ->
  KillOne = fun(P) ->
    case is_process_alive(P) of
      true  -> exit(P, kill);
      false -> ok
    end
  end,
  lists:map(KillOne, Procs),
  ok.
  
%% Make a list of all monitored processes
-spec get_procs(Tab) -> Procs
  when Tab   :: ets:tab(),
       Procs :: [pid()].
       
get_procs(Tab) ->
  ets:foldl(
   fun({P, true}, Acc) -> [P|Acc] end,
   [], Tab
  ).
  
