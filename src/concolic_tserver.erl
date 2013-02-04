%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_tserver).
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

-type state() :: #state{}.
-type call_request() :: {'register_parent', pid()} | {'is_monitored', pid()}
                      | {'node_servers', node()} | {'get_fd', pid()}.
-type call_reply() :: {'ok', file:name()} | boolean()
                    | {'ok', {pid(), pid()}} | {'ok', file:io_device()}.
-type cast_request() :: {'store_fd', pid(), file:io_device()} | {'terminate', pid()}.
-type info_request() :: {'DOWN', reference(), 'process', pid(), term()}.

%%====================================================================
%% External exports
%%====================================================================

%% Initialize a TraceServer
-spec init_traceserver(TraceDir :: string(), Super :: pid()) -> TraceServer :: pid().

init_traceserver(TraceDir, Super) ->
  case gen_server:start(?MODULE, [TraceDir, Super], []) of
    {ok, TraceServer} -> TraceServer;
    {error, Reason}   -> exit({traceserver_init, Reason})
  end.

%% Terminate a TraceServer
-spec terminate(TraceServer :: pid()) -> 'ok'.

terminate(TraceServer) ->
  gen_server:cast(TraceServer, {terminate, self()}).

%% Register a new process so that the TraceServer can monitor it
-spec register_to_trace(TraceServer :: pid(), Parent :: pid()) -> {'ok', file:io_device()}.

register_to_trace(TraceServer, Parent) ->
  {ok, Filename} = gen_server:call(TraceServer, {register_parent, Parent}),
  {ok, File} = concolic_encdec:create_file(Filename),
  store_file_descriptor(TraceServer, File),
  ok = concolic_encdec:log_term(File, {pid, self()}),
  %%  ok = concolic_encdec:close_file(File),
  {ok, File}.

%% Check if a process is monitored by TraceServer
-spec is_monitored(TraceServer :: pid(), Who :: pid()) -> boolean().

is_monitored(TraceServer, Who) ->
  gen_server:call(TraceServer, {is_monitored, Who}).

%% Request the CodeServer and TraceServer of a specific node
-spec node_servers(TraceServer :: pid(), Node :: node()) -> {pid(), pid()}.

node_servers(TraceServer, Node) ->
  {ok, Servers} = gen_server:call(TraceServer, {node_servers, Node}),
  Servers.

%% Request the file descriptor of a process' trace file
-spec file_descriptor(TraceServer :: pid()) -> file:io_device().

file_descriptor(TraceServer) ->
  {ok, Fd} = gen_server:call(TraceServer, {get_fd, self()}),
  Fd.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% ---------------------------------------------------------------------
%% gen_server callback : init/1
%% ---------------------------------------------------------------------
-spec init([(string() | pid())]) -> {'ok', state()}.

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
  
%% ---------------------------------------------------------------------
%% gen_server callback : terminate/2
%% ---------------------------------------------------------------------
-spec terminate(term(), State :: state()) -> 'ok'.

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
  ok = concolic:send_tlogs(Super, Logs).

%% ---------------------------------------------------------------------
%% gen_server callback : code_change/3
%% ---------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
%% ---------------------------------------------------------------------
%% gen_server callback : handle_call/3
%% ---------------------------------------------------------------------
-spec handle_call(Req :: call_request(),{From :: pid(), Tag :: reference()}, State :: state()) ->
  {'reply', Reply :: call_reply(), NewState :: state()}.
  
%% Call Request : {register_parent, Parent, Link}
%% Ret Msg : {ok, Filename}
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
  %% io:format("[~s(~p)]: Monitoring ~p~n", [?MODULE, node(), FromPid]),
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
  Servers = concolic:node_servers(Super, Node),
  {reply, {ok, Servers}, State};
  
%% Call Request : {get_fd, Who}
%% Ret Msg : {ok, FileDescriptor}
handle_call({get_fd, Who}, _From, State) ->
  Fds = State#state.fds,
  [{Who, Fd}] = ets:lookup(Fds, Who),
  {reply, {ok, Fd}, State}.

%% ---------------------------------------------------------------------
%% gen_server callback : handle_cast/2
%% ---------------------------------------------------------------------
-spec handle_cast(Req :: cast_request(), State :: state()) ->
  {'noreply', NewState :: state()} | {'stop', 'normal', NewState :: state()}.

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

%% ---------------------------------------------------------------------
%% gen_server callback : handle_info/2
%% ---------------------------------------------------------------------
-spec handle_info(Msg :: info_request(), State :: state()) ->
  {'noreply', NewState :: state()} | {'stop', 'normal', NewState :: state()}.

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
  concolic:send_error_report(Super, Who, concolic_eval:unzip_error(Reason)),
  {stop, normal, State}.
  
%%====================================================================
%% Internal functions
%%====================================================================

%% Stores the file descriptor of a process's trace
-spec store_file_descriptor(TraceServer :: pid(), Fd :: file:io_device()) -> 'ok'.

store_file_descriptor(TraceServer, Fd) ->
  gen_server:cast(TraceServer, {store_fd, self(), Fd}).

%% Kill all monitored processes
-spec kill_all_processes(Procs :: [pid()]) -> 'ok'.
  
kill_all_processes(Procs) ->
  KillOne = 
    fun(P) ->
      case is_process_alive(P) of
        true  -> exit(P, kill);
        false -> ok
      end
    end,
  lists:foreach(KillOne, Procs).
  
%% Make a list of all monitored processes
-spec get_procs(Tab :: ets:tab()) -> Procs :: [pid()].
       
get_procs(Tab) ->
  ets:foldl(fun({P, true}, Acc) -> [P|Acc] end, [], Tab).
