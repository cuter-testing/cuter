%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(python).
-behaviour(gen_fsm).

%% External exports
-export([start/0, exec/2, load_file/2, check_model/1, get_model/1,
         stop/1, solve/4]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4,
         %% custom state names
         idle/2, idle/3, waiting/2, waiting/3, solving/2, solving/3,
         solved/2, solved/3, generating_model/2, generating_model/3,
         finished/2, finished/3]).

%% fsm state datatype
-record(state, {
  super,
  from = null,
  port = null
}).

-type reply() :: {reply, ok, statename(), state()}
               | {stop, term(), ok, state()}.
-type ret() :: {stop, term(), state()}
             | {next_state, statename(), state()}.
-type state() :: #state{}.
-type statename() :: idle | waiting | solving | solved | generating_model | finished.


%% ============================================================================
%% External exports
%% ============================================================================

%% Start the Python fsm
-spec start() -> pid() | {error, term()}.

start() ->
  case gen_fsm:start_link(?MODULE, self(), []) of
    {ok, Pid} -> Pid;
    {error, _Reason} = R -> R
  end.

%% Execute a Python command
-spec exec(pid(), string()) -> ok.

exec(Pid, Python) ->
  gen_fsm:sync_send_event(Pid, {exec, Python}).

%% Port Command: Load a trace file
-spec load_file(pid(), {file:name(), integer(), integer()}) -> ok.

load_file(Pid, FileInfo) ->
  gen_fsm:sync_send_event(Pid, {load_file, FileInfo}).

%% Port Command: Check the model for satisfiability
-spec check_model(pid()) -> binary().

check_model(Pid) ->
  gen_fsm:sync_send_event(Pid, check_model, 500000).

%% Port Command: Get the instance of the sat model
-spec get_model(pid()) -> binary().

get_model(Pid) ->
  gen_fsm:sync_send_event(Pid, get_model, 500000).

%% Stop the Python fsm
-spec stop(pid()) -> ok.

stop(Pid) ->
  gen_fsm:sync_send_event(Pid, stop).

%% Interact with Z3 to solve a set of constraints
%% SIMPLIFICATION - Assume Sequential program
-spec solve(file:name(), integer(), [concolic_symbolic:mapping()], string()) -> {ok, [term()]} | error.

solve(File, I, Mapping, Python) ->
  FSM = python:start(),
  python:exec(FSM, Python),
  python:load_file(FSM, {File, 1, I}),
  Sat = python:check_model(FSM),
  case Sat of
    <<"sat">> ->
      M = python:get_model(FSM),
      Decoded = concolic_json:decode_z3_result(M),
      Inp = concolic_symbolic:generate_new_input(Mapping, Decoded),
      python:stop(FSM),
      {ok, Inp};
    _ ->
      python:stop(FSM),
      error
  end.

%% ============================================================================
%% gen_fsm callbacks
%% ============================================================================

%% ------------------------------------------------------------------
%% gen_fsm callback : init/1
%% ------------------------------------------------------------------
-spec init(pid()) -> {ok, statename(), state()}.

init(Super) ->
  process_flag(trap_exit, true),
  {ok, idle, #state{super = Super}}.

%% ------------------------------------------------------------------
%% gen_fsm callback : terminate/3
%% ------------------------------------------------------------------
-spec terminate(term(), statename(), state()) -> ok.

terminate(normal, finished, _Data) ->
  ok;
terminate(Reason, State,  #state{port = Port}) ->
  case erlang:port_info(Port) of
    undefined -> ok;
    _ -> port_close(Port)
  end,
  exit({State, Reason}).

%% ------------------------------------------------------------------
%% gen_fsm callback : code_change/4
%% ------------------------------------------------------------------
-spec code_change(term(), statename(), state(), term()) -> {ok, statename(), state()}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%% ------------------------------------------------------------------
%% gen_fsm callback : handle_event/3
%% ------------------------------------------------------------------
-spec handle_event(term(), statename(), state()) -> ret().

handle_event(Event, _StateName, Data) ->
  {stop, {unexpected_event, Event}, Data}.

%% ------------------------------------------------------------------
%% gen_fsm callback : handle_sync_event/4
%% ------------------------------------------------------------------
-spec handle_sync_event(term(), tuple(), statename(), state()) -> ret() | reply().

handle_sync_event(Event, _From, _StateName, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% ------------------------------------------------------------------
%% gen_fsm callback : StateName/2, StateName/3
%% ------------------------------------------------------------------

%% State 'idle'
-spec idle(term(), state()) -> ret().
idle(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec idle(term(), tuple(), state()) -> reply().
idle({exec, Python}, _From, Data) ->
  Port = open_port({spawn, Python}, [{packet, 2}, binary, hide]),
  {reply, ok, waiting, Data#state{port = Port}};
idle(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% State 'waiting'
-spec waiting(term(), state()) -> ret().
waiting(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec waiting(term(), tuple(), state()) -> ret() | reply().
waiting({load_file, FileInfo}, _From, Data=#state{port = Port}) ->
  Cmd = concolic_json:prepare_port_command(load_file, FileInfo),
  Port ! {self(), {command, Cmd}},
  {reply, ok, waiting, Data};
waiting(check_model, From, Data=#state{port = Port}) ->
  Cmd = concolic_json:prepare_port_command(check_model, null),
  Port ! {self(), {command, Cmd}},
  {next_state, solving, Data#state{from = From}};
waiting(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, Data}.

%% State 'solving'
-spec solving(term(), state()) -> ret().
solving(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec solving(term(), tuple(), state()) ->  reply().
solving(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% State 'solved'
-spec solved(term(), state()) -> ret().
solved(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec solved(term(), tuple(), state()) -> ret() | reply().
solved(get_model, From, Data=#state{port = Port}) ->
  Cmd = concolic_json:prepare_port_command(get_model, null),
  Port ! {self(), {command, Cmd}},
  {next_state, generating_model, Data#state{from = From}};
solved(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% State 'generating_model'
-spec generating_model(term(), state()) -> ret().
generating_model(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec generating_model(term(), tuple(), state()) -> ret() | reply().
generating_model(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% State 'finished'
-spec finished(term(), state()) -> ret().
finished(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec finished(term(), tuple(), state()) -> ret() | reply().
finished(stop, _From, Data) ->
  {stop, normal, ok, Data};
finished(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% ------------------------------------------------------------------
%% gen_fsm callback : handle_info/3
%% ------------------------------------------------------------------
-spec handle_info(term(), statename(), state()) -> ret().

handle_info({Port, {data, Bin}}, solving, Data=#state{from = From, port = Port}) ->
  gen_fsm:reply(From, Bin),
  case Bin of
    <<"sat">> -> {next_state, solved, Data#state{from = null}};
    _ -> {next_state, finished, Data#state{from = null}}
  end;
handle_info({Port, {data, Bin}}, generating_model, Data=#state{from = From, port = Port}) ->
  gen_fsm:reply(From, Bin),
  {next_state, finished, Data#state{from = null}};
handle_info(Info, _StateName, Data) ->
  {stop, {unexpected_info, Info}, Data}.






