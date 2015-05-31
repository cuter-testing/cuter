%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_monitor).
-behaviour(gen_server).

%% external exports
-export([start/4, stop/1, subscribe/2, is_monitored/2, file_descriptor/1, node_servers/2,
         %% Access logs
         dir_of_logs/1, procs_of_logs/1]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).

-include("include/cuter_macros.hrl").

-export_type([logs/0]).

%% The logs of a MonitorServer.
-record(logs, {
  dir   :: file:filename(),
  procs :: integer()
}).
-type logs() :: #logs{}.

%% Server's state
%% ---------------
%%
%% super :: pid()
%%   The supervisor process that spawned the monitor server.
%% depth :: integer()
%%   Number of branches to log per process.
%%   Passes this information to newly spawned processes.
%% dir :: nonempty_string()
%%   The directory where the generated .core files are stored.
%% procs :: ets:tab()
%%   Pids of alive interpreter processes.
%% fds :: ets:tab()
%%   Stores the file descriptor of the log file of each interepreter process.
%%   ETS table with elements: {Pid, Fd}
%% ptree :: ets:tab()
%%   Stores the execution process graph in the node
%%   ETS table with elements: {Parent :: pid(), Child :: pid()}
%% prefix :: nonempty_string()
%%   A prefix unique to the concolic execution.
%%   Used to prefix named processes & ets tables.
%% logs :: logs()
%%   Store information logs

-record(st, {
  super  :: pid(),
  depth  :: integer(),
  dir    :: string(),
  procs  :: ets:tab(),
  fds    :: ets:tab(),
  ptree  :: ets:tab(),
  prefix :: nonempty_string(),
  logs   :: logs()
}).
-type state() :: #st{}.

%% ============================================================================
%% External exports (Public API)
%% ============================================================================

%% Start a monitor server
-spec start(nonempty_string(), pid(), pos_integer(), nonempty_string()) -> pid().
start(LogDir, Super, Depth, Prefix) ->
  Args = [LogDir, Super, Depth, Prefix],
  case gen_server:start(?MODULE, Args, []) of
    {ok, MonitorServer} -> MonitorServer;
    {error, Reason} -> exit({monitorserver_start, Reason})
  end.

%% Stop a monitor server
-spec stop(pid()) -> ok.
stop(MonitorServer) ->
  gen_server:cast(MonitorServer, {stop, self()}).

%% Register a new process so that the TraceServer can monitor it
-spec subscribe(pid(), pid()) -> {ok, file:io_device()}.
subscribe(MonitorServer, Parent) ->
  {ok, Filename, Depth, Prefix} = gen_server:call(MonitorServer, {subscribe, Parent}),
  {ok, Fd} = cuter_log:open_file(Filename, write),
  store_file_descriptor(MonitorServer, Fd),
  put(?DEPTH_PREFIX, Depth), %% Set Remaining Constraint counter to Depth
  put(?EXECUTION_PREFIX, Prefix), %% Set the execution prefix
  {ok, Fd}.

%% Check if a process is monitored by TraceServer
-spec is_monitored(pid(), pid()) -> boolean().
is_monitored(MonitorServer, Who) ->
  gen_server:call(MonitorServer, {is_monitored, Who}).

%% Request the file descriptor of a process' trace file
-spec file_descriptor(pid()) -> file:io_device().
file_descriptor(MonitorServer) ->
  {ok, Fd} = gen_server:call(MonitorServer, {get_fd, self()}),
  Fd.

%% Request the monitor code servers of a specific node
-spec node_servers(pid(), node()) -> servers().
node_servers(MonitorServer, Node) ->
  {ok, Servers} = gen_server:call(MonitorServer, {node_servers, Node}),
  Servers.

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([nonempty_string() | pid() | pos_integer(), ...]) -> {ok, state()}.
init([Dir, Super, Depth, Prefix]) when is_list(Dir) ->
  process_flag(trap_exit, true),
  link(Super),
  Ptree = ets:new(?MODULE, [bag, protected]),
  Fds = ets:new(?MODULE, [ordered_set, protected]),
  Procs = ets:new(?MODULE, [ordered_set, protected]),
  LogDir = cuter_lib:get_monitor_dir(Dir),
  InitState = #st{ super = Super
                 , depth = Depth
                 , dir = LogDir
                 , procs = Procs
                 , fds = Fds
                 , ptree = Ptree
                 , prefix = Prefix
                 , logs = mk_logs(LogDir)},
  {ok, InitState}.

%% gen_server callback : terminate/2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #st{fds = Fds, logs = Logs, procs = Procs, ptree = Ptree, super = Super}) ->
  %% TODO reconstruct Process Tree and Traces Tree
  ets:delete(Ptree),
  ets:delete(Procs),
  ets:delete(Fds),
  %% Send Logs to supervisor
  ok = cuter_iserver:monitor_logs(Super, Logs),
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call({subscribe, pid()}, {pid(), any()}, state()) -> {reply, {ok, file:filename_all(), pos_integer(), nonempty_string()}, state()}
               ; ({is_monitored, pid()}, {pid(), any()}, state()) -> {reply, boolean(), state()}
               ; ({get_fd, pid()}, {pid(), any()}, state()) -> {reply, {ok, file:io_device()}, state()}
               ; ({node_servers, node()}, {pid(), any()}, state()) -> {reply, {ok, (servers() | error)}, state()}.
handle_call({subscribe, Parent}, {From, _FromTag}, State=#st{depth = Depth, dir = Dir, logs = Logs, prefix = Prefix, procs = Procs, ptree = Ptree}) ->
  monitor(process, From),
  ets:insert(Ptree, {Parent, From}),
  ets:insert(Procs, {From}),
  %% Update Logs - Number of monitored processes
  NewLogs = Logs#logs{procs = Logs#logs.procs + 1},
  %% Create the filename of the log file
  Filename = cuter_lib:logfile_name(Dir, From),
  ok = filelib:ensure_dir(Filename),  % Ensure that the directory exists
  {reply, {ok, Filename, Depth, Prefix}, State#st{logs = NewLogs}};
handle_call({is_monitored, Who}, {_From, _FromTag}, State=#st{procs = Procs}) ->
  Monitored = 
    case ets:lookup(Procs, Who) of
      [] -> false;
      [{Who}] -> true
    end,
  {reply, Monitored, State};
handle_call({get_fd, Who}, _From, State=#st{fds = Fds}) ->
  [{Who, Fd}] = ets:lookup(Fds, Who),
  {reply, {ok, Fd}, State};
handle_call({node_servers, Node}, _From, State=#st{super = Super}) ->
  Servers = cuter_iserver:node_servers(Super, Node),
  {reply, {ok, Servers}, State}.


%% gen_server callback : handle_cast/2
-spec handle_cast({store_fd, pid(), file:io_device()}, state()) -> {noreply, state()}
               ; ({stop, pid()}, state()) -> {stop, normal, state()} | {noreply, state()}.
handle_cast({store_fd, From, Fd}, State=#st{fds = Fds}) ->
  ets:insert(Fds, {From, Fd}),
  {noreply, State};
handle_cast({stop, FromWho}, State=#st{procs = Procs, super = Super}) ->
  case FromWho =:= Super of
    true ->
      kill_all_processes(get_procs(Procs)),
      {stop, normal, State};
    false ->
      {noreply, State}
  end.

%% gen_server callback : handle_info/2
%% Msg when a monitored process exited normally
-spec handle_info({'DOWN', reference(), process, pid(), any()}, state()) -> {stop, normal, state()} | {noreply, state()}.
handle_info({'DOWN', _Ref, process, Who, normal}, State=#st{fds = Fds, procs = Procs}) ->
  ets:delete(Fds, Who),
  ets:delete(Procs, Who),
  case ets:first(Procs) of
    '$end_of_table' -> {stop, normal, State};
    _  -> {noreply, State}
  end;
%% Msg when a monitored process experienced an exception
handle_info({'DOWN', _Ref, process, Who, Reason}, State=#st{fds = Fds, procs = Procs, super = Super}) ->
  ets:delete(Procs, Who),
  ets:delete(Fds, Who),
  %% Killing all remaining alive processes
  kill_all_processes(get_procs(Procs)),
  %% Send the Error Report to the supervisor
  cuter_iserver:send_error_report(Super, Who, cuter_eval:unzip_error(Reason)),
  {stop, normal, State}.

%% ----------------------------------------------------------------------------
%% Manage the logs of the CodeServer
%% ----------------------------------------------------------------------------

-spec mk_logs(file:filename()) -> logs().
mk_logs(Dir) ->
  #logs{dir = Dir, procs = 0}.

-spec dir_of_logs(logs()) -> file:filename().
dir_of_logs(Logs) ->
  Logs#logs.dir.

-spec procs_of_logs(logs()) -> integer().
procs_of_logs(Logs) ->
  Logs#logs.procs.

%% ============================================================================
%% Internal functions (Helper methods)
%% ============================================================================

%% Stores the file descriptor of a process's trace
-spec store_file_descriptor(pid(), file:io_device()) -> ok.
store_file_descriptor(MonitorServer, Fd) ->
  gen_server:cast(MonitorServer, {store_fd, self(), Fd}).

%% Make a list of all monitored processes
-spec get_procs(ets:tab()) -> [pid()].
get_procs(Tab) ->
  ets:foldl(fun({P}, Acc) -> [P|Acc] end, [], Tab).

%% Kill all monitored processes
-spec kill_all_processes([pid()]) -> ok.
kill_all_processes(Procs) ->
  lists:foreach(fun kill_process/1, Procs).

kill_process(P) ->
  case is_process_alive(P) of
    true -> exit(P, kill);
    false -> ok
  end.

