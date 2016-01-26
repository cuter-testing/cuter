%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_iserver).
-behaviour(gen_server).

-include("include/cuter_macros.hrl").

%% external exports
-export([start/6, node_servers/2, int_return/2, send_error_report/3, monitor_logs/2, send_mapping/2,
         %% Access logs
         logs_foreach/2, int_of_node_logs/1, mapping_of_node_logs/1, monitorLogs_of_node_logs/1,
         mapping_of_logs/1, monitorLogs_of_logs/1, int_of_logs/1]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).

-export_type([execution_status/0, logs/0, node_logs/0, maybe/1, int_process/0]).

%% Internal type declarations
-type ann_pid() :: {node(), pid()}.
-type int_process() :: {node(), pid()}.
-type execution_status() :: {success, cuter_eval:result()} 
                          | {internal_error, internal_error()}
                          | {runtime_error, runtime_error()}.

-type server()         :: 'codeserver' | 'monitorserver' | 'iserver'.
-type internal_error() :: {server(), node(), any()}.
-type runtime_error()  :: {node(), pid(), cuter_eval:result()}.

-type maybe(X) :: X | undefined.

-record(nlogs, {
  int                :: maybe(int_process()),
  mapping            :: maybe([cuter_symbolic:mapping()]),
  monitorLogs        :: maybe(cuter_monitor:logs()),
  monitorServerError :: any(),
  result             :: any(),
  runtimeError       :: maybe(runtime_error())
}).
-type node_logs() :: #nlogs{}.

-type logs()         :: dict:dict(node(), node_logs()).
-type monitor_logs() :: [{node(), cuter_monitor:logs()}].
-type mapping_logs() :: [{node(), maybe([cuter_symbolic:mapping()])}].
-type int_logs()     :: [{node(), maybe(int_process())}].

%% Server's state
%% ---------------
%%
%% codeServer :: pid()
%%   The main CodeServer.
%% depth :: cuter:depth()
%%   Number of branches to log per process.
%%   Intended for the MonitorServer.
%% exstatus :: execution_status()
%%   Stores the status of the concolic execution.
%% info :: logs()
%%   Stores information about the concolic execution.
%% int :: {pid(), running} | exited
%%   The 1st interpreted process and its status.
%% logdir :: nonempty_string()
%%   Directory to store log files.
%%   Intended for the MonitorServer.
%% mpids :: [{node(), pid()}]
%%   Proplists of all the MonitorServers.
%% prefix :: nonempty_string()
%%   Unique prefix for the concolic execution.
%%   Intended for the MonitorServer.
%% super :: pid()
%%   The coordinator process that spawned the InterpreterServer.

-record(st, {
  codeServer :: pid(),
  depth      :: cuter:depth(),
  exstatus   :: execution_status() | 'undefined',
  info       :: logs(),
  int        :: {pid(), 'running'} | 'exited',
  logdir     :: nonempty_string(),
  mpids      :: [{node(), pid()}],
  prefix     :: nonempty_string(),
  super      :: pid()
}).
-type state() :: #st{}.

%% ============================================================================
%% External exports (Public API)
%% ============================================================================

%% Starts the interpreter server.
-spec start(module(), atom(), [any()], nonempty_string(), pos_integer(), pid()) -> pid().
start(M, F, As, LogDir, Depth, CodeServer) ->
  Args = [M, F, As, LogDir, Depth, CodeServer, self()],
  case gen_server:start(?MODULE, Args, []) of
    {ok, IServer} -> IServer;
    {error, Reason} -> exit({iserver_start, Reason})
  end.

%% Request the code server and the monitor server of a specific node
-spec node_servers(pid(), node()) -> servers() | error.
node_servers(IServer, Node) ->
  gen_server:call(IServer, {node_servers, Node}).

%% Store the result of the first interpreted process
-spec int_return(pid(), any()) -> ok | mismatch.
int_return(IServer, Return) ->
  gen_server:call(IServer, {int_return, Return}).

%% Send the report of a runtime error
-spec send_error_report(pid(), pid(), cuter_eval:result()) -> ok.
send_error_report(IServer, Who, Error) ->
  gen_server:call(IServer, {error_report, Who, Error}).

%% Send the logs of a monitor server
-spec monitor_logs(pid(), cuter_monitor:logs()) -> ok.
monitor_logs(IServer, Logs) ->
  gen_server:call(IServer, {monitor_logs, Logs}).

%% Send the mapping of the concrete to symbolic values
-spec send_mapping(pid(), [cuter_symbolic:mapping()]) -> ok | mismatch.
send_mapping(IServer, Mapping) ->
  gen_server:call(IServer, {mapping, Mapping}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks (Server Implementation)
%% ----------------------------------------------------------------------------

%% gen_server callback : init/1
-spec init([cuter:mod() | atom() | cuter:input() | nonempty_string() | cuter:depth() | pid()]) -> {ok, state()}.
init([M, F, As, LogDir, Depth, CodeServer, Super]) ->
  link(Super),
  process_flag(trap_exit, true),
  Node = node(),
  Prefix = cuter_lib:unique_string() ++ "_",
  MonitorServer = cuter_monitor:start(LogDir, self(), Depth, Prefix),
  Servers = #svs{code = CodeServer, monitor = MonitorServer},
  Ipid = cuter_eval:i(M, F, As, Servers),
  InitState = #st{ codeServer = CodeServer
                 , depth = Depth
                 , info = dict:store(Node, #nlogs{int = {node(Ipid), Ipid}}, dict:new())
                 , int = {Ipid, running}
                 , logdir = LogDir
                 , mpids = [{Node, MonitorServer}]
                 , prefix = Prefix
                 , super = Super},
  {ok, InitState}.

%% gen_server callback : terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #st{super = Super, info = Logs, exstatus = ExStatus}) ->
  Super ! {self(), ExStatus, Logs},
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call({int_return, any()}, {pid(), any()}, state()) -> {reply, (ok | mismatch), state()}
               ; ({mapping, [cuter_symbolic:mapping()]}, {pid(), any()}, state()) -> {reply, (ok | mismatch), state()}
               ; ({error_report, pid(), cuter_eval:result()}, {pid(), any()}, state()) -> {reply, ok, state()}
               ; ({node_servers, node()}, {pid(), any()}, state()) -> {reply, servers(), state()} | {stop, error, normal, state()}
               ; ({monitor_logs, cuter_monitor:logs()}, {pid(), any()}, state()) -> {reply, ok, state()}.
%% Log the result of the first spawned process
handle_call({int_return, Return}, {From, _FromTag}, S=#st{int = Ipid, info = Info, exstatus = ExStatus}) ->
  case Ipid =:= {From, running} of %% Ipid is always a local process
    false ->
      {reply, mismatch, S};
    true ->
      Info1 = dict:update(node(), fun(Logs) -> Logs#nlogs{result = Return} end, Info),
      case ExStatus of 
        undefined ->
          {reply, ok, S#st{info = Info1, int = exited, exstatus = {success, Return}}};
        _ ->
          {reply, ok, S#st{info = Info1, int = exited}}
      end
  end;
%% Log the mapping of concrete to symbolic values
handle_call({mapping, Mapping}, {From, _FromTag}, S=#st{int = Ipid, info = Info}) ->
  case Ipid =:= {From, running} of
    true ->
      Info1 = dict:update(node(), fun(Logs) -> Logs#nlogs{mapping = Mapping} end, Info),
      {reply, ok, S#st{info = Info1}};
    false ->
      {reply, mismatch, S}
  end;
%% Log a runtime error
handle_call({error_report, Who, Error}, {From, _FromTag}, S=#st{mpids = Ms, info = Info}) ->
  {Node, From} = NF = lists:keyfind(From, 2, Ms),  %% Process will never be registered
  %% Store Runtime Error's Info
  RuntimeError = {Node, Who, Error},
  Info1 = dict:update(Node, fun(Logs) -> Logs#nlogs{runtimeError = RuntimeError} end, Info),
  %% Shutdown execution tree
  force_terminate(Ms -- [NF]),
  ExStatus = {runtime_error, RuntimeError},
  {reply, ok, S#st{info = Info1, exstatus = ExStatus}};
%% A request for the servers of a specific node
handle_call({node_servers, Node}, {_From, _FromTag}, S=#st{codeServer = CServer, mpids = Ms, logdir = Dir, depth = Depth, prefix = Prefix, info = I}) ->
  case node_monitored(Node, CServer, Ms) of
    {true, Servers} ->
      %% Servers are already up on the Node
      {reply, Servers, S};
    false ->
      %% No servers up on the Node / need to spawn them
      case spawn_remote_servers(Node, self(), Dir, Depth, Prefix, CServer) of
        error ->
          %% Error while spawning servers so shutdown processes and
          %% terminate immediately with internal error
          force_terminate(Ms),
          Error = {internal_error, {iserver, node(), spawn_remote_servers}},
          {stop, error, normal, S#st{exstatus = Error}};
        {ok, Servers} ->
          NMs = [{Node, Servers#svs.monitor} | Ms],
          Info = dict:store(Node, #nlogs{}, I),
          {reply, Servers, S#st{mpids = NMs, info = Info}}
      end
  end;
%% Store the logs of a monitor server
handle_call({monitor_logs, Logs}, {From, _FromTag}, S=#st{mpids = Ms, info = Info}) ->
  {Node, From} = lists:keyfind(From, 2, Ms),  %% Process will never be registered
  Info1 = dict:update(Node, fun(Ls) -> Ls#nlogs{monitorLogs = Logs} end, Info),
  {reply, ok, S#st{info = Info1}}.

%% gen_server callback : handle_cast/2
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.  %% Nothing planned for cast requests

%% gen_server callback : handle_info/2
-spec handle_info({'EXIT', pid(), normal}, state()) -> {stop, normal, state()} | {noreply, state()}.
%% When a code server or monitor server have exited normally
handle_info({'EXIT', Who, normal}, S=#st{mpids = Ms}) ->
  case locate(Who, Ms) of
    false -> {noreply, S};
    {true, Node} ->
      %% A monitor server exited
      NMs = Ms -- [{Node, Who}],
      NState = S#st{mpids = NMs},
      case execution_completed(NState) of
        true ->
          %% If all processes have finished then terminate
          {stop, normal, NState};
        false ->
          %% else wait for the rest
          {noreply, NState}
      end
  end;
%% When a CodeServer or TraceServer have exited with exceptions
handle_info({'EXIT', Who, Reason}, S=#st{mpids = Ms, info = Info}) ->
  %% locate the server that exited and shutdown the execution tree
  case locate(Who, Ms) of
    false ->
      {noreply, S};
    {true, Node} ->
      NMs = Ms -- [{Node, Who}],
      force_terminate(NMs),
      Info1 = dict:update(Node, fun(Logs) -> Logs#nlogs{monitorServerError = Reason} end, Info),
      Exstatus = {internal_error, {monitorserver, Node, Reason}},
      NState = S#st{mpids = NMs, info = Info1, exstatus = Exstatus},
      case execution_completed(NState) of
        true  -> {stop, normal, NState};
        false -> {noreply, NState}
      end
  end.

%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------

%% Check whether a node has been assigned a MonitorServer.
-spec node_monitored(node(), pid(), [ann_pid()]) -> {true, servers()} | false.
node_monitored(Node, CodeServer, Ms) ->
  case lists:keyfind(Node, 1, Ms) of
    false -> false;
    {Node, MonitorServer} -> {true, #svs{code = CodeServer, monitor = MonitorServer}}
  end.

%% Spawns a MonitorServer at a remote node.
-spec spawn_remote_servers(node(), pid(), nonempty_string(), cuter:depth(), nonempty_string(), pid()) -> {ok, servers()} | error.
spawn_remote_servers(Node, Super, LogDir, Depth, Prefix, CodeServer) ->
  Me = self(),
  Setup = fun() ->
    process_flag(trap_exit, true),
    MonitorServer = cuter_monitor:start(LogDir, Super, Depth, Prefix),
    Me ! {self(), #svs{code = CodeServer, monitor = MonitorServer}}
  end,
  Pid = spawn_link(Node, Setup),
  receive
    {Pid, Servers} -> {ok, Servers};
    {'EXIT', Pid, _Error} -> error
  after
    5000 -> error
  end.

%% Sends terminate requests to all the live MonitorServers.
-spec force_terminate([ann_pid()]) -> ok.
force_terminate(Ms) ->
  lists:foreach(fun({_Node, P}) -> cuter_monitor:stop(P) end, Ms).

%% Locates and returns the node info of a MonitorServer.
-spec locate(pid(), [ann_pid()]) -> {true, node()} | false.
locate(Who, Ms) ->
  case lists:keyfind(Who, 2, Ms) of
    false -> false;
    {Node, Who} -> {true, Node}
  end.

%% Determines whether the concolic execution has ended or not.
-spec execution_completed(state()) -> boolean().
execution_completed(#st{mpids = Ms}) -> Ms =:= [].

%% ----------------------------------------------------------------------------
%% Manage the logs of the InterpreterServer
%% ----------------------------------------------------------------------------

-spec logs_foreach(fun((node(), node_logs()) -> ok), logs()) -> ok.
logs_foreach(Fun, Logs) ->
  Fold = fun(Node, NodeLogs, _Ok) -> Fun(Node, NodeLogs) end,
  dict:fold(Fold, ok, Logs).

-spec int_of_logs(logs()) -> int_logs().
int_of_logs(Logs) ->
  dict:to_list(dict:map(fun(_Node, NodeLogs) -> NodeLogs#nlogs.int end, Logs)).

-spec mapping_of_logs(logs()) -> mapping_logs().
mapping_of_logs(Logs) ->
  dict:to_list(dict:map(fun(_Node, NodeLogs) -> NodeLogs#nlogs.mapping end, Logs)).

-spec monitorLogs_of_logs(logs()) -> monitor_logs().
monitorLogs_of_logs(Logs) ->
  dict:to_list(dict:map(fun(_Node, NodeLogs) -> NodeLogs#nlogs.monitorLogs end, Logs)).

-spec int_of_node_logs(node_logs()) -> int_process().
int_of_node_logs(Logs) ->
  Logs#nlogs.int.

-spec mapping_of_node_logs(node_logs()) -> maybe([cuter_symbolic:mapping()]).
mapping_of_node_logs(Logs) ->
  Logs#nlogs.mapping.

-spec monitorLogs_of_node_logs(node_logs()) -> cuter_monitor:logs().
monitorLogs_of_node_logs(Logs) ->
  Logs#nlogs.monitorLogs.
