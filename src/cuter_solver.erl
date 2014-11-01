%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver).
-behaviour(gen_fsm).

-include("cuter_macros.hrl").

%% External exports
-export([start/0, lookup_in_model/2, run/4]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4,
         %% custom state names
         idle/2, idle/3, python_started/2, python_started/3,
         trace_loaded/2, trace_loaded/3, axioms_added/2, axioms_added/3,
         solved/2, solved/3, generating_model/2, generating_model/3,
         expecting_var/2, expecting_var/3, expecting_value/2, expecting_value/3,
         model_received/2, model_received/3, finished/2, finished/3,
         failed/2, failed/3
       ]).

-export_type([model/0]).

%% fsm state datatype
-record(fsm_state, {
  super,
  from = null,
  port = null,
  sol = #{},
  var = null
}).

-type state()     :: idle | python_started | trace_loaded | axioms_added | solving 
                   | failed | solved | generating_model | expecting_var | expecting_value 
                   | model_received | finished.
-type fsm_state() :: #fsm_state{}.
-type from()      :: {pid(), reference()}.
-type err_async() :: {stop, {unexpected_event, term()}, fsm_state()}.
-type err_sync()  :: {stop, {unexpected_event, term()}, ok, fsm_state()}.
-type model()     :: #{cuter_symbolic:symbolic() => any()}.

%% ============================================================================
%% External exports
%% ============================================================================

initial_setting(N) -> <<0:N>>.

next_setting(Setting, N) ->
  case Setting of
    <<0:N>> -> <<1:N>>;
    <<X:N>> -> <<(X bsl 1):N >>
  end.

is_limit_setting(Setting, N) ->
  Max = << <<1:1>> || _ <- lists:seq(1,N) >>,
  Overflow = <<0:N>>,
  case Setting of
    Max -> true;
    Overflow -> true;
    _ -> false
  end.

generate_next_setting(Setting, N) ->
  Next = next_setting(Setting, N),
  case is_limit_setting(Next, N) of
    true  -> error;
    false -> {ok, Next}
  end.


-spec run(string(), [cuter_symbolic:mapping()], file:name(), integer()) -> [any()] | error.
run(Python, Mappings, File, N) ->
  FSM = start(),
  ok = exec(FSM, Python),
  ok = load_trace_file(FSM, {File, N}),
  Setting = initial_setting(length(Mappings)),
  query_solver(FSM, Mappings, Setting).

query_solver(FSM, Mappings, CurrSetting) ->
  ok = add_axioms(FSM),
  load_setting(CurrSetting, Mappings, FSM),
  case check_model(FSM) of
    true -> get_solution(FSM, Mappings);
    false ->
      NextSetting = generate_next_setting(CurrSetting, length(Mappings)),
      query_with_new_setting(NextSetting, FSM, Mappings)
  end.

query_with_new_setting(error, FSM, _Mappings) ->
  ok = stop_exec(FSM),
  wait_for_fsm(FSM, error);
query_with_new_setting({ok, Setting}, FSM, Mappings) ->
  ok = reset_solver(FSM),
  query_solver(FSM, Mappings, Setting).

load_setting(<<>>, [], _FSM) ->
  ok;
load_setting(<<F:1, Rest/bitstring>>, [M|Ms], FSM) ->
  case F of
    0 -> load_setting(Rest, Ms, FSM);
    1 ->
      fix_variable(FSM, M),
      load_setting(Rest, Ms, FSM)
  end.

get_solution(FSM, Mappings) ->
  M = get_model(FSM),
  ok = stop_exec(FSM),
  Inp = cuter_symbolic:generate_new_input(Mappings, M),
  wait_for_fsm(FSM, Inp).

wait_for_fsm(FSM, Ret) ->
  receive 
    {'EXIT', FSM, normal} -> Ret
  after 2000 ->
    io:format("TIMEOUT~n"), Ret
  end.

%% Lookup the value of a symbolic var in the generated model
-spec lookup_in_model(cuter_symbolic:symbolic(), map()) -> any().
lookup_in_model(Var, Model) -> maps:get(Var, Model).

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

%% Load a trace file
-spec load_trace_file(pid(), {file:name(), integer()}) -> ok.
load_trace_file(Pid, FileInfo) ->
  gen_fsm:sync_send_event(Pid, {load_trace_file, FileInfo}, 10000).

%% Add the generated axioms to the solver
-spec add_axioms(pid()) -> ok.
add_axioms(Pid) ->
  gen_fsm:sync_send_event(Pid, add_axioms).

%% Fix a variable to a specific value
-spec fix_variable(pid(), cuter_symbolic:mapping()) -> ok.
fix_variable(Pid, Mapping) ->
  gen_fsm:sync_send_event(Pid, {fix_variable, Mapping}).

%% Check the model for satisfiability
-spec check_model(pid()) -> boolean().
check_model(Pid) ->
  gen_fsm:sync_send_event(Pid, check_model, 500000).

%% Get the instance of the sat model
-spec get_model(pid()) -> [cuter_symbolic:mapping()].
get_model(Pid) ->
  gen_fsm:sync_send_event(Pid, get_model, 500000).

-spec stop_exec(pid()) -> ok.
stop_exec(Pid) ->
  gen_fsm:sync_send_event(Pid, stop_exec).

-spec reset_solver(pid()) -> ok.
reset_solver(Pid) ->
  gen_fsm:sync_send_event(Pid, reset_solver).

%% ============================================================================
%% gen_fsm callbacks (FSM Implementation)
%% ============================================================================

%% gen_fsm callback : init/1
-spec init(pid()) -> {ok, state(), fsm_state()}.
init(Super) ->
  process_flag(trap_exit, true),
  {ok, idle, #fsm_state{super = Super}}.

%% gen_fsm callback : terminate/3
-spec terminate(term(), state(), fsm_state()) -> ok.
terminate(normal, finished, _Data) ->
  ok;
terminate(Reason, State, #fsm_state{port = Port}) ->
  case erlang:port_info(Port) of
    undefined -> ok;
    _ -> port_close(Port)
  end,
  exit({State, Reason}).

%% gen_fsm callback : code_change/4
-spec code_change(term(), state(), fsm_state(), term()) -> {ok, state(), fsm_state()}.
code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%% gen_fsm callback : handle_event/3
-spec handle_event(term(), state(), fsm_state()) -> err_async().
handle_event(Event, _StateName, Data) ->
  {stop, {unexpected_event, Event}, Data}.

%% gen_fsm callback : handle_sync_event/4
-spec handle_sync_event(any(), from(), state(), fsm_state()) -> err_sync().
handle_sync_event(Event, _From, _StateName, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


%% [State] idle
-spec idle(any(), fsm_state()) -> err_async().
idle(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec idle(any(), from(), fsm_state()) -> {reply, ok, waiting, fsm_state()} | err_sync().
idle({exec, Python}, _From, Data) ->
  Port = open_port({spawn, Python}, [{packet, 2}, binary, hide]),
  io:format("[FSM] Started FSM ~p~n", [Port]),
  {reply, ok, python_started, Data#fsm_state{port = Port}};
idle(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


%% [State] python_started
-spec python_started(any(), fsm_state()) -> err_async().
python_started(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec python_started(any(), from(), fsm_state()) -> {reply, ok, trace_loaded, fsm_state()} | err_sync().
python_started({load_trace_file, FileInfo}, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(load_trace_file, FileInfo),
  io:format("[FSM] load_trace_file~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, trace_loaded, Data};
python_started(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


%% [State] trace_loaded
-spec trace_loaded(any(), fsm_state()) -> err_async().
trace_loaded(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec trace_loaded(any(), from(), fsm_state()) -> {reply, ok, axioms_added, fsm_state()} | err_sync().
trace_loaded(add_axioms, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(add_axioms, nil),
  io:format("[FSM] add_axioms~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, axioms_added, Data};
trace_loaded(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


%% [State] axioms_added
-spec axioms_added(any(), fsm_state()) -> err_async().
axioms_added(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec axioms_added(any(), from(), fsm_state()) -> {next_state, solving, fsm_state()}  |err_sync().
axioms_added(check_model, From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(solve, nil),
  io:format("[FSM] check_model~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {next_state, solving, Data#fsm_state{from = From}};
axioms_added({fix_variable, Mapping}, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(fix_variable, Mapping),
  io:format("[FSM] fix_variable~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, axioms_added, Data};
axioms_added(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


%% [State] failed
-spec failed(any(), fsm_state()) -> err_async().
failed(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec failed(any(), from(), fsm_state()) -> {reply, ok, finished, fsm_state()} | err_sync().
failed(reset_solver, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(reset_solver, nil),
  io:format("[FSM] reset_solver~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, trace_loaded, Data};
failed(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(stop, nil),
  io:format("[FSM] stop_exec~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, finished, Data};
failed(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] solved
-spec solved(any(), fsm_state()) -> err_async().
solved(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec solved(any(), from(), fsm_state()) -> {next_state, generating_model, fsm_state()} | err_sync().
solved(get_model, From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(get_model, nil),
  io:format("[FSM] get_model~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {next_state, generating_model, Data#fsm_state{from = From}};
solved(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] generating_model
-spec generating_model(any(), fsm_state()) -> err_async().
generating_model(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec generating_model(any(), from(), fsm_state()) -> err_sync().
generating_model(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] expecting_var
-spec expecting_var(any(), fsm_state()) -> err_async().
expecting_var(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec expecting_var(any(), from(), fsm_state()) -> err_sync().
expecting_var(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] expecting_value
-spec expecting_value(any(), fsm_state()) -> err_async().
expecting_value(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec expecting_value(any(), from(), fsm_state()) -> err_sync().
expecting_value(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] model_received
-spec model_received(any(), fsm_state()) -> err_async().
model_received(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec model_received(any(), from(), fsm_state()) -> {reply, ok, finished, fsm_state()} | err_sync().
model_received(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(stop, nil),
  io:format("[FSM] stop_exec~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, finished, Data};
model_received(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% [State] finished
-spec finished(any(), fsm_state()) -> err_async().
finished(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec finished(any(), from(), fsm_state()) -> err_sync().
finished(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.




%% gen_fsm callback : handle_info/3
-spec handle_info({port(), {data, binary()}}, state(), fsm_state()) -> {next_state, state(), fsm_state()}
                                                                     | {stop, {unexpected_info, any()}, fsm_state()}.

handle_info({Port, {data, <<"True">>}}, solving, Data=#fsm_state{from = From, port = Port}) ->
  io:format("[PORT $] @solving SAT~n"),
  gen_fsm:reply(From, true),
  {next_state, solved, Data#fsm_state{from = null}};
handle_info({Port, {data, <<"False">>}}, solving, Data=#fsm_state{from = From, port = Port}) ->
  io:format("[PORT $] @solving NOT SAT~n"),
  gen_fsm:reply(From, false),
  {next_state, failed, Data#fsm_state{from = null}};
handle_info({Port, {data, ?RSP_MODEL_DELIMITER_START}}, generating_model, Data=#fsm_state{port = Port}) ->
  io:format("[PORT $] @generating_model model_start~n"),
  {next_state, expecting_var, Data};
handle_info({Port, {data, ?RSP_MODEL_DELIMITER_END}}, expecting_var, Data=#fsm_state{from = From, port = Port, sol = S}) ->
  io:format("[PORT $] @expecting_var model_end~n"),
  gen_fsm:reply(From, S),
  {next_state, model_received, Data#fsm_state{from = null, sol = []}};
handle_info({Port, {data, Bin}}, expecting_var, Data=#fsm_state{port = Port}) ->
  try cuter_json:json_to_term(Bin) of
    Var ->
      io:format("[PORT $] @expecting_var VAR: ~p~n", [Var]),
      {next_state, expecting_value, Data#fsm_state{var = Var}}
  catch _:_ ->
    io:format("[PORT] @expecting_var ~p~n", [Bin]),
    {next_state, expecting_var, Data}
  end;
handle_info({Port, {data, Bin}}, expecting_value, Data=#fsm_state{port = Port, var = Var, sol = S}) ->
  try cuter_json:json_to_term(Bin) of
    Val ->
      io:format("[PORT $] @expecting_value VAL: ~p~n", [Val]),
      Sol_n = maps:put(Var, Val, S),
      {next_state, expecting_var, Data#fsm_state{var = null, sol = Sol_n}}
  catch _:_ ->
    io:format("[PORT] @expecting_value ~p~n", [Bin]),
    {next_state, expecting_value, Data}
  end;
handle_info({'EXIT', Port, normal}, finished, Data=#fsm_state{port = Port}) ->
  io:format("[PORT] @finished exited~n"),
  {stop, normal, Data#fsm_state{port = null}};
handle_info({Port, {data, Bin}}, State, Data=#fsm_state{port = Port}) ->
  io:format("[PORT] @~p ~p~n", [State, Bin]),
  {next_state, State, Data};
handle_info(Info, _StateName, Data) ->
  {stop, {unexpected_info, Info}, Data}.






