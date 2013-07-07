%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_tserver).
-behaviour(gen_server).

%% External exports
-export([init_traceserver/3, terminate/1, register_to_trace/2,
         is_monitored/2, node_servers/2, file_descriptor/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
         code_change/3, handle_info/2, handle_cast/2]).
         
%% exported types
-export_type([tlogs/0]).

%% type declarations
-type call()  :: {'register_parent', pid()}
               | {'is_monitored', pid()}
               | {'node_servers', node()}
               | {'get_fd', pid()}.
-type cast()  :: {'store_fd', pid(), file:io_device()}
               | {'terminate', pid()}.
-type info()  :: {'DOWN', reference(), 'process', pid(), term()}.
-type reply() :: {'ok', file:name(), integer()}
               | boolean()
               | {'ok', {pid(), pid()}}
               | {'ok', file:io_device()}.
%% gen_server state datatype
-record(state, {
  super :: pid(),      %% Concolic Server (supervisor) process
  depth :: integer(),  %% Number of constraints to log
  procs :: ets:tab(),  %% Pids of Live Evaluator processes
  ptree :: ets:tab(),  %% ETS table where {Parent, Child} process pids are stored
  fds   :: ets:tab(),  %% ETS table where {Pid, Fd} are stored
  dir   :: string(),   %% Directory where traces are saved
  logs  :: tlogs()     %% Proplist to store log informations // currently only {procs, NumOfMonitoredProcs}
}).
-type state() :: #state{}.
-type tlogs() :: [proplists:property()].

-define(DEPTH_PREFIX, '__conc_depth').

%% ============================================================================
%% External exports
%% ============================================================================

%% Initialize a TraceServer
-spec init_traceserver(string(), pid(), integer()) -> pid() | no_return().

init_traceserver(TraceDir, Super, Depth) ->
  case gen_server:start(?MODULE, [TraceDir, Super, Depth], []) of
    {ok, TraceServer} -> TraceServer;
    {error, Reason}   -> exit({traceserver_init, Reason})
  end.

%% Terminate a TraceServer
-spec terminate(pid()) -> 'ok'.

terminate(TraceServer) ->
  gen_server:cast(TraceServer, {terminate, self()}).

%% Register a new process so that the TraceServer can monitor it
-spec register_to_trace(pid(), pid()) -> {'ok', file:io_device()}.

register_to_trace(TraceServer, Parent) ->
  {ok, Filename, Depth} = gen_server:call(TraceServer, {register_parent, Parent}),
  {ok, Fd} = concolic_encdec:open_file(Filename, 'write'),
  store_file_descriptor(TraceServer, Fd),
  put(?DEPTH_PREFIX, Depth), %% Set Remaining Constraint counter to Depth
%  ok = concolic_encdec:log_pid(Fd, self()),
  {ok, Fd}.

%% Check if a process is monitored by TraceServer
-spec is_monitored(pid(), pid()) -> boolean().

is_monitored(TraceServer, Who) ->
  gen_server:call(TraceServer, {is_monitored, Who}).

%% Request the CodeServer and TraceServer of a specific node
-spec node_servers(pid(), node()) -> concolic:servers().

node_servers(TraceServer, Node) ->
  {ok, Servers} = gen_server:call(TraceServer, {node_servers, Node}),
  Servers.

%% Request the file descriptor of a process' trace file
-spec file_descriptor(pid()) -> file:io_device().

file_descriptor(TraceServer) ->
  {ok, Fd} = gen_server:call(TraceServer, {get_fd, self()}),
  Fd.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ------------------------------------------------------------------
%% gen_server callback : init/1
%% ------------------------------------------------------------------
-spec init([string() | pid() | integer(), ...]) -> {'ok', state()}.

init([Dir, Super, Depth]) ->
  process_flag(trap_exit, true),
  link(Super),
  Ptree = ets:new(?MODULE, [bag, protected]),
  Fds = ets:new(?MODULE, [ordered_set, protected]),
  Procs = ets:new(?MODULE, [ordered_set, protected]),
  U = erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>",
  TraceDir = filename:absname(Dir ++ "/trace-" ++ U),
  ok = filelib:ensure_dir(TraceDir ++ "/"),  %% Create the directory
  InitState = #state{
    super = Super,
    depth = Depth,
    procs = Procs,
    ptree = Ptree,
    fds = Fds,
    dir = TraceDir,
    logs = [{procs, 0}, {dir, TraceDir}]
  },
  {ok, InitState}.
  
%% ------------------------------------------------------------------
%% gen_server callback : terminate/2
%% ------------------------------------------------------------------
-spec terminate(term(), state()) -> 'ok'.

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

%% ------------------------------------------------------------------
%% gen_server callback : code_change/3
%% ------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.
  
%% ------------------------------------------------------------------
%% gen_server callback : handle_call/3
%% ------------------------------------------------------------------
-spec handle_call(call(), {pid(), reference()}, state()) -> {'reply', reply(), state()}.
  
%% Call Request : {register_parent, Parent, Link}
%% Ret Msg : {ok, Filename, Depth}
handle_call({register_parent, Parent}, {From, _FromTag}, State) ->
  Procs = State#state.procs,
  Ptree = State#state.ptree,
  Dir = State#state.dir,
  Logs = State#state.logs,
  Depth = State#state.depth,
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
  P = proplists:get_value(procs, Logs),
  NewLogs = [{procs, P+1}|(Logs -- [{procs, P}])],
  %% Create the filename of the log file
  F = erlang:pid_to_list(FromPid) -- "<>",
  Filename = filename:absname(Dir ++ "/proc-" ++ F),
  {reply, {ok, Filename, Depth}, State#state{logs=NewLogs}};
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

%% ------------------------------------------------------------------
%% gen_server callback : handle_cast/2
%% ------------------------------------------------------------------
-spec handle_cast(cast(), state()) -> {'noreply', state()} | {'stop', 'normal', state()}.
  
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

%% ------------------------------------------------------------------
%% gen_server callback : handle_info/2
%% ------------------------------------------------------------------
-spec handle_info(info(), state()) -> {'noreply', state()} | {'stop', 'normal', state()}.

%% Msg when a monitored process exited normally
handle_info({'DOWN', _Ref, process, Who, normal}, State) ->
  Procs = State#state.procs,
  Fds = State#state.fds,
  ets:delete(Fds, Who),
  ets:delete(Procs, Who),
  case ets:first(Procs) of
    '$end_of_table' -> {stop, normal, State};
    _  -> {noreply, State}
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
  
%% ============================================================================
%% Internal functions
%% ============================================================================

%% Stores the file descriptor of a process's trace
-spec store_file_descriptor(pid(), file:io_device()) -> 'ok'.

store_file_descriptor(TraceServer, Fd) ->
  gen_server:cast(TraceServer, {store_fd, self(), Fd}).

%% Kill all monitored processes
-spec kill_all_processes([pid()]) -> 'ok'.
  
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
-spec get_procs(ets:tab()) -> [pid()].
  
get_procs(Tab) ->
  ets:foldl(fun({P, true}, Acc) -> [P|Acc] end, [], Tab).
  
