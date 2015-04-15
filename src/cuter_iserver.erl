%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_iserver).
-behaviour(gen_server).

-include("include/cuter_macros.hrl").

%% external exports
-export([start/7, node_servers/2, int_return/2, send_error_report/3, code_logs/2, monitor_logs/2, send_mapping/2]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).

-export_type([execution_status/0]).

%% Internal type declarations
-type ann_pid() :: {node(), pid()}.
-type execution_status() :: {success, cuter_eval:result()} 
                          | {internal_error, (codeserver | monitorserver | iserver), node(), any()}
                          | {runtime_error, node(), pid(), cuter_eval:result()}.

%% Server's state
%% ---------------
%%
%% super :: pid()
%%   The coordinator process that spawned the monitor server.
%% logdir :: nonempty_string()
%%   Directory to store log files.
%%   Intended for the monitor server.
%% depth :: integer()
%%   Number of constraints to log per process.
%%   Intended for the monitor server.
%% prefix :: nonempty_string()
%%   Unique prefix for the concolic execution.
%%   Intended for the monitor server.
%% cpids :: orddict:orddict()
%%   Proplists of all the code servers
%% mpids :: orddict:orddict()
%%   Proplists of all the monitor servers
%% info :: orddict:orddict()
%%   Stores information about the concolic execution
%% int :: {pid(), running} | exited
%%   The 1st interpreted process and its status
%% exstatus :: execution_status()
%%   Stores the status of the concolic execution

-record(state, {
  super         :: pid(),
  logdir        :: nonempty_string(),
  depth         :: integer(),
  prefix        :: nonempty_string(),
  cpids         :: orddict:orddict(), %% [{node(), pid()}]
  mpids         :: orddict:orddict(), %% [{node(), pid()}]
  info          :: orddict:orddict(), %% [{node(), list()}]
  int           :: {pid(), running} | exited,
  exstatus      :: execution_status(),
  stored_mods   :: cuter_analyzer:stored_modules(),
  tags_added_no :: integer()
}).
-type state() :: #state{}.

%% ============================================================================
%% External exports (Public API)
%% ============================================================================

%% Start the interpreter server
-spec start(module(), atom(), [any()], nonempty_string(), pos_integer(), cuter_analyzer:stored_modules(), integer()) ->
              {ok, state()} | execution_status().
start(M, F, As, LogDir, Depth, StoredMods, TagsN) ->
  Args = [M, F, As, LogDir, Depth, StoredMods, TagsN, self()],
  case gen_server:start(?MODULE, Args, []) of
    {ok, IServer} -> IServer;
    {error, Reason} -> internal_error(iserver, Reason)
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
-spec send_error_report(pid(), pid(), term()) -> ok.
send_error_report(IServer, Who, Error) ->
  gen_server:call(IServer, {error_report, Who, Error}).

%% Send the logs of a code server
-spec code_logs(pid(), logs()) -> ok.
code_logs(IServer, Logs) ->
  gen_server:call(IServer, {code_logs, Logs}).

%% Send the logs of a monitor server
-spec monitor_logs(pid(), logs()) -> ok.
monitor_logs(IServer, Logs) ->
  gen_server:call(IServer, {monitor_logs, Logs}).

%% Send the mapping of the concrete to symbolic values
-spec send_mapping(pid(), [cuter_symbolic:mapping()]) -> ok | mismatch.
send_mapping(IServer, Mapping) ->
  gen_server:call(IServer, {mapping, Mapping}).

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([module() | atom() | [any()] | nonempty_string() | integer() | cuter_analyzer:stored_modules(), ...]) -> {ok, state()}.
init([M, F, As, LogDir, Depth, StoredMods, TagsN, Super]) ->
  link(Super),
  process_flag(trap_exit, true),
  Node = node(),
  Prefix = cuter_lib:unique_string() ++ "_",
  CodeServer = cuter_codeserver:start(self(), StoredMods, TagsN),
  MonitorServer = cuter_monitor:start(LogDir, self(), Depth, Prefix),
  Servers = #svs{code = CodeServer, monitor = MonitorServer},
  Ipid = cuter_eval:i(M, F, As, Servers),
  InitState = #state{
    super = Super,
    logdir = LogDir,
    depth = Depth,
    prefix = Prefix,
    cpids = [{Node, CodeServer}],
    mpids = [{Node, MonitorServer}],
    info = orddict:store(Node, [{int, {node(Ipid), Ipid}}], orddict:new()),
    int = {Ipid, running},
    stored_mods = StoredMods,
    tags_added_no = TagsN
  },
  {ok, InitState}.

%% gen_server callback : terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, State) ->
  Super = State#state.super,
  Info = State#state.info,
  Exstatus = State#state.exstatus,
  Super ! {self(), Exstatus, Info},
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call({int_return, any()}, {pid(), any()}, state()) -> {reply, (ok | mismatch), state()}
               ; ({mapping, [cuter_symbolic:mapping()]}, {pid(), any()}, state()) -> {reply, (ok | mismatch), state()}
               ; ({error_report, pid(), any()}, {pid(), any()}, state()) -> {reply, ok, state()}
               ; ({node_servers, node()}, {pid(), any()}, state()) -> {reply, servers(), state()} | {stop, error, normal, state()}
               ; ({code_logs, logs()}, {pid(), any()}, state()) -> {reply, ok, state()}
               ; ({monitor_logs, logs()}, {pid(), any()}, state()) -> {reply, ok, state()}.
%% Log the result of the first spawned process
handle_call({int_return, Return}, {From, _FromTag}, State) ->
  Ipid = State#state.int,
  Info = State#state.info,
  case Ipid =:= {From, running} of %% Ipid is always a local process
    false ->
      {reply, mismatch, State};
    true ->
      NInfo = orddict:append(node(), {result, Return}, Info),
      case State#state.exstatus of 
        undefined ->
          ExStatus = {success, Return},
          {reply, ok, State#state{info = NInfo, int = exited, exstatus = ExStatus}};
        _ ->
          {reply, ok, State#state{info = NInfo, int = exited}}
      end
  end;
%% Log the mapping of concrete to symbolic values
handle_call({mapping, Mapping}, {From, _FromTag}, State) ->
  case State#state.int =:= {From, running} of
    true ->
      Info = orddict:append(node(), {mapping, Mapping}, State#state.info),
      {reply, ok, State#state{info = Info}};
    false ->
      {reply, mismatch, State}
  end;
%% Log a runtime error
handle_call({error_report, Who, Error}, {From, _FromTag}, State) ->
  Cs = State#state.cpids,
  Ms = State#state.mpids,
  {Node, From} = NF = lists:keyfind(From, 2, Ms),  %% Process will never be registered
  %% Store Runtime Error's Info
  Info = orddict:append(Node, {runtime_error, {Node, Who, Error}}, State#state.info),
  %% Shutdown execution tree
  force_terminate(Cs, Ms -- [NF]),
  ExStatus = {runtime_error, Node, Who, Error},
  {reply, ok, State#state{info = Info, exstatus = ExStatus}};
%% A request for the servers of a specific node
handle_call({node_servers, Node}, {_From, _FromTag}, State) ->
  Cs = State#state.cpids,
  Ms = State#state.mpids,
  case node_monitored(Node, Cs, Ms) of
    {true, Servers} ->
      %% Servers are already up on the Node
      {reply, Servers, State};
    false ->
      %% No servers up on the Node / need to spawn them
      LogDir = State#state.logdir,
      Depth = State#state.depth,
      Prefix = State#state.prefix,
      StoredMods = State#state.stored_mods,
      TagsN = State#state.tags_added_no,
      case spawn_remote_servers(Node, self(), LogDir, Depth, Prefix, StoredMods, TagsN) of
        error ->
          %% Error while spawning servers so shutdown processes and
          %% terminate immediately with internal error
          force_terminate(Cs, Ms),
          Error = internal_error(iserver, spawn_remote_servers),
          {stop, error, normal, State#state{exstatus = Error}};
        {ok, Servers = Servers} ->
          NCs = [{Node, Servers#svs.code} | Cs],
          NMs = [{Node, Servers#svs.monitor} | Ms],
          Info = orddict:store(Node, [], State#state.info),
          {reply, Servers, State#state{cpids = NCs, mpids = NMs, info = Info}}
      end
  end;
%% Store the logs of a code server
handle_call({code_logs, Logs}, {From, _FromTag}, State) ->
  Cs = State#state.cpids,
  {Node, From} = lists:keyfind(From, 2, Cs),  %% Process will never be registered
  Info = orddict:append(Node, {code_logs, Logs}, State#state.info),
  {reply, ok, State#state{info = Info}};
%% Store the logs of a monitor server
handle_call({monitor_logs, Logs}, {From, _FromTag}, State) ->
  Ms = State#state.mpids,
  {Node, From} = lists:keyfind(From, 2, Ms),  %% Process will never be registered
  Info = orddict:append(Node, {monitor_logs, Logs}, State#state.info),
  {reply, ok, State#state{info = Info}}.

%% gen_server callback : handle_cast/2
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.  %% Nothing planned for cast requests

%% gen_server callback : handle_info/2
-spec handle_info({'EXIT', pid(), normal}, state()) -> {stop, normal, state()} | {noreply, state()}.
%% When a code server or monitor server have exited normally
handle_info({'EXIT', Who, normal}, State) ->
  Cs = State#state.cpids,
  Ms = State#state.mpids,
  case locate(Who, Cs, Ms) of
    false -> {noreply, State};
    {true, codeserver, Node} ->
      %% A code server exited
      NCs = Cs -- [{Node, Who}],
      NState = State#state{cpids = NCs},
      case execution_completed(NState) of
        true  -> {stop, normal, NState};
        false -> {noreply, NState}
      end;
    {true, monitorserver, Node} ->
      %% A monitor server exited
      NMs = Ms -- [{Node, Who}],
      NState = State#state{mpids = NMs},
      case execution_completed(NState) of
        true ->
          %% If all processes have finished then terminate
          {stop, normal, NState};
        false ->
          %% else stop the code server of the node and wait for the rest
          {Node, CodeServer} = lists:keyfind(Node, 1, Cs),
          cuter_codeserver:stop(CodeServer),
          {noreply, NState}
      end
  end;
%% When a CodeServer or TraceServer have exited with exceptions
handle_info({'EXIT', Who, Reason}, State) ->
  Cs = State#state.cpids,
  Ms = State#state.mpids,
  Info = State#state.info,
  %% locate the server that exited and shutdown the execution tree
  case locate(Who, Cs, Ms) of
    false ->
      {noreply, State};
    {true, codeserver, Node} ->
      NCs = Cs -- [{Node, Who}],
      force_terminate(NCs, Ms),
      NInfo = orddict:append(Node, {codeserver_error, Reason}, Info),
      Exstatus = {internal_error, codeserver, Node, Reason},
      NState = State#state{cpids = NCs, info = NInfo, exstatus = Exstatus},
      case execution_completed(NState) of
        true  -> {stop, normal, NState};
        false -> {noreply, NState}
      end;
    {true, monitorserver, Node} ->
      NMs = Ms -- [{Node, Who}],
      force_terminate(Cs, NMs),
      NInfo = orddict:append(Node, {monitorserver_error, Reason}, Info),
      Exstatus = {internal_error, monitorserver, Node, Reason},
      NState = State#state{mpids = NMs, info = NInfo, exstatus = Exstatus},
      case execution_completed(NState) of
        true  -> {stop, normal, NState};
        false -> {noreply, NState}
      end
  end.


%% ============================================================================
%% Internal functions (Helper methods)
%% ============================================================================

%% Create the representation of an internal error
internal_error(Type, Error) -> {internal_error, Type, node(), Error}.

%% Check whether a node has beend assigned a code and a monitor server
-spec node_monitored(node(), [ann_pid()], [ann_pid()]) -> {true, servers()} | false.
node_monitored(Node, Cs, Ms) ->
  case {lists:keyfind(Node, 1, Cs), lists:keyfind(Node, 1, Ms)} of
    {false, false} -> false;
    {{Node, CodeServer}, {Node, MonitorServer}} -> {true, #svs{code = CodeServer, monitor = MonitorServer}}
  end.

%% Spawn a code server and a monitor server at a remote node
-spec spawn_remote_servers(node(), pid(), nonempty_string(), pos_integer(), nonempty_string(), cuter_analyzer:stored_modules(), integer()) ->
        {ok, servers()} | error.
spawn_remote_servers(Node, Super, LogDir, Depth, Prefix, StoredMods, TagsN) ->
  Me = self(),
  Setup = fun() ->
    process_flag(trap_exit, true),
    CodeServer = cuter_codeserver:start(Super, StoredMods, TagsN),
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

%% Send terminate requests to all the live code serves and monitor servers
-spec force_terminate([ann_pid()], [ann_pid()]) -> ok.
force_terminate(Cs, Ms) ->
  lists:foreach(fun({_Node, P}) -> cuter_codeserver:stop(P) end, Cs),
  lists:foreach(fun({_Node, P}) -> cuter_monitor:stop(P) end, Ms).

%% Locate and return the node info of a CodeServer or TraceServer
-spec locate(pid(), [ann_pid()], [ann_pid()]) -> {true, (codeserver | monitorserver), node()} | false.
locate(Who, Cs, Ms) ->
  case lists:keyfind(Who, 2, Cs) of
    {Node, Who} -> {true, codeserver, Node};
    false ->
      case lists:keyfind(Who, 2, Ms) of
        false -> false;
        {Node, Who} -> {true, monitorserver, Node}
      end
  end.

%% Determine whether the concolic execution has ended or not
-spec execution_completed(state()) -> boolean().
execution_completed(#state{cpids = Cs, mpids = Ts}) ->
  case {Cs, Ts} of
    {[], []} -> true;
    _ -> false
  end.
