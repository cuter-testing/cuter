%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver).
-behaviour(gen_fsm).

-include("cuter_macros.hrl").

-export([
  %% external exports
    start/0
  , lookup_in_model/2
  , solve/4
  %% gen_fsm callbacks
  , init/1
  , handle_event/3
  , handle_sync_event/4
  , handle_info/3
  , terminate/3
  , code_change/4
  %% FSM states
  , idle/2
  , idle/3
  , python_started/2
  , python_started/3
  , trace_loaded/2
  , trace_loaded/3
  , axioms_added/2
  , axioms_added/3
  , solved/2
  , solved/3
  , generating_model/2
  , generating_model/3
  , expecting_var/2
  , expecting_var/3
  , expecting_value/2
  , expecting_value/3
  , model_received/2
  , model_received/3
  , finished/2
  , finished/3
  , failed/2
  , failed/3
]).

-export_type([model/0]).

-type state()     :: idle
                   | python_started
                   | trace_loaded
                   | axioms_added
                   | solving
                   | failed
                   | solved
                   | generating_model
                   | expecting_var
                   | expecting_value
                   | model_received
                   | finished.
-type from()      :: {pid(), reference()}.
-type err_async() :: {stop, {unexpected_event, any()}, fsm_state()}.
-type err_sync()  :: {stop, {unexpected_event, any()}, ok, fsm_state()}.
-type model()     :: #{cuter_symbolic:symbolic() => any()}.
-type solver_result() :: [any()] | error.
-type simplifier_conf() :: bitstring().

%% fsm state datatype
-record(fsm_state, {
  super         :: pid(),
  from = null   :: (from() | null),
  port = null   :: (port() | null),
  sol = #{}     :: model(),
  var = null    :: (cuter_symbolic:symbolic() | null)
}).
-type fsm_state() :: #fsm_state{}.

%% ----------------------------------------------------------------------------
%% Query the Z3 SMT Solver
%% ----------------------------------------------------------------------------

-spec solve(string(), [cuter_symbolic:mapping()], file:name(), integer()) -> solver_result().
solve(Python, Mappings, File, N) ->
  FSM = start(),
  ok = exec(FSM, Python),
  ok = load_trace_file(FSM, {File, N}),
  Setting = initial_setting(length(Mappings)),
  query_solver(FSM, Mappings, Setting).


-spec query_solver(pid(), [cuter_symbolic:mapping()], bitstring()) -> solver_result().
query_solver(FSM, Mappings, CurrSetting) ->
  ok = add_axioms(FSM),
  load_setting(CurrSetting, Mappings, FSM),
  case check_model(FSM) of
    true ->
      get_solution(FSM, Mappings);
    false ->
      NextSetting = generate_next_setting(CurrSetting, length(Mappings)),
      query_with_new_setting(NextSetting, FSM, Mappings)
  end.

-spec get_solution(pid(), [cuter_symbolic:mapping()]) -> solver_result().
get_solution(FSM, Mappings) ->
  M = get_model(FSM),
  ok = stop_exec(FSM),
  Inp = cuter_symbolic:generate_new_input(Mappings, M),
  wait_for_fsm(FSM, Inp).

-spec wait_for_fsm(pid(), solver_result()) -> solver_result().
wait_for_fsm(FSM, Ret) ->
  receive {'EXIT', FSM, normal} -> Ret
  after 500 -> io:format("TIMEOUT~n"), Ret
  end.

-spec query_with_new_setting(error, pid(), [cuter_symbolic:mapping()]) -> error
                          ; ({ok, simplifier_conf()}, pid(), [cuter_symbolic:mapping()]) -> solver_result().
query_with_new_setting(error, FSM, _Mappings) ->
  ok = stop_exec(FSM),
  wait_for_fsm(FSM, error);
query_with_new_setting({ok, Setting}, FSM, Mappings) ->
  ok = reset_solver(FSM),
  query_solver(FSM, Mappings, Setting).

%% Lookup the value of a symbolic var in the generated model
-spec lookup_in_model(cuter_symbolic:symbolic(), model()) -> any().
lookup_in_model(Var, Model) ->
  maps:get(Var, Model).

%% ----------------------------------------------------------------------------
%% Manage the setting for the query.
%% The setting is a bit string with length N, where N is the number of
%% the symbolic parameters.
%% If the i-th bit is 1 then we fix the i-th parameter to its existing value,
%% else we leave it unbound.
%% This technique is used to simplify the query to the solver.
%% ----------------------------------------------------------------------------

%% The initial setting.
%% Do not fix any variable.
-spec initial_setting(integer()) -> simplifier_conf().
initial_setting(N) -> <<0:N>>.

%% Generate the next setting if possible.
-spec generate_next_setting(simplifier_conf(), integer()) -> {ok, simplifier_conf()} | error.
generate_next_setting(Setting, N) ->
  Next = next_setting(Setting, N),
  case is_limit_setting(Next, N) of
    true  -> error;
    false -> {ok, Next}
  end.

%% Actually generate the next setting.
%% The current strategy is to have at most one variable fixed,
%% so we just apply a left bit shift to the setting.
-spec next_setting(simplifier_conf(), integer()) -> simplifier_conf().
next_setting(Setting, N) ->
  case Setting of
    <<0:N>> -> <<1:N>>;
    <<X:N>> -> <<(X bsl 1):N >>
  end.

%% Check if we have generated all the available settings.
%% For our current strategy, the end is when we have performed
%% N left bit shifts, where N is the length of the bitstring.
-spec is_limit_setting(simplifier_conf(), integer()) -> boolean().
is_limit_setting(Setting, N) ->
  Max = << <<1:1>> || _ <- lists:seq(1,N) >>,
  Overflow = <<0:N>>,
  case Setting of
    Max -> true;
    Overflow -> true;
    _ -> false
  end.

%% Load the current setting to the solver.
-spec load_setting(simplifier_conf(), [cuter_symbolic:mapping()], pid()) -> ok.
load_setting(<<>>, [], _FSM) ->
  ok;
load_setting(<<F:1, Rest/bitstring>>, [M|Ms], FSM) ->
  case F of
    0 ->
      load_setting(Rest, Ms, FSM);
    1 ->
      fix_variable(FSM, M),
      load_setting(Rest, Ms, FSM)
  end.

%% ----------------------------------------------------------------------------
%% API to interact with the FSM
%% ----------------------------------------------------------------------------

%% Start the FSM
-spec start() -> pid() | {error, term()}.
start() ->
  case gen_fsm:start_link(?MODULE, self(), []) of
    {ok, Pid} -> Pid;
    {error, _Reason} = R -> R
  end.

%% Execute an external program
%% In this case, it will be a Python program.
-spec exec(pid(), string()) -> ok.
exec(Pid, Python) ->
  gen_fsm:sync_send_event(Pid, {exec, Python}).

%% Load the trace file
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

%% Remove all the axioms from the solver
-spec reset_solver(pid()) -> ok.
reset_solver(Pid) ->
  gen_fsm:sync_send_event(Pid, reset_solver).

%% Stop the FSM
-spec stop_exec(pid()) -> ok.
stop_exec(Pid) ->
  gen_fsm:sync_send_event(Pid, stop_exec).

%% ----------------------------------------------------------------------------
%% gen_fsm callbacks
%% ----------------------------------------------------------------------------

%% init/1
-spec init(pid()) -> {ok, state(), fsm_state()}.
init(Super) ->
  process_flag(trap_exit, true),
  {ok, idle, #fsm_state{super = Super}}.

%% terminate/3
-spec terminate(term(), state(), fsm_state()) -> ok.
terminate(normal, finished, _Data) ->
  ok;
terminate(Reason, State, #fsm_state{port = Port}) ->
  %% Ensure the port has closed
  case erlang:port_info(Port) of
    undefined -> ok;
    _ -> port_close(Port)
  end,
  exit({State, Reason}).

%% code_change/4
-spec code_change(any(), state(), fsm_state(), any()) -> {ok, state(), fsm_state()}.
code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%% handle_event/3
-spec handle_event(any(), state(), fsm_state()) -> err_async().
handle_event(Event, _StateName, Data) ->
  {stop, {unexpected_event, Event}, Data}.

%% handle_sync_event/4
-spec handle_sync_event(any(), from(), state(), fsm_state()) -> err_sync().
handle_sync_event(Event, _From, _StateName, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%% handle_info/3
%% Useful to handle messages from the port
-spec handle_info({port(), {data, binary()}}, state(), fsm_state()) -> {next_state, state(), fsm_state()}
                                                                     | {stop, {unexpected_info, any()}, fsm_state()}.
%% The model is satisfiable and the solver has generated a solution
%% solving --> solved
handle_info({Port, {data, <<"True">>}}, solving, Data=#fsm_state{from = From, port = Port}) ->
  io:format("[PORT $] @solving SAT~n"),
  gen_fsm:reply(From, true),
  {next_state, solved, Data#fsm_state{from = null}};
%% The solver did not manage to satisfy the model
%% solving --> failed
handle_info({Port, {data, <<"False">>}}, solving, Data=#fsm_state{from = From, port = Port}) ->
  io:format("[PORT $] @solving NOT SAT~n"),
  gen_fsm:reply(From, false),
  {next_state, failed, Data#fsm_state{from = null}};
%% Delimit the start of the solution
%% generating_model --> expecting_var
handle_info({Port, {data, ?RSP_MODEL_DELIMITER_START}}, generating_model, Data=#fsm_state{port = Port}) ->
  io:format("[PORT $] @generating_model model_start~n"),
  {next_state, expecting_var, Data};
%% Delimit the end of the solution
%% expecting_var --> model_received
handle_info({Port, {data, ?RSP_MODEL_DELIMITER_END}}, expecting_var, Data=#fsm_state{from = From, port = Port, sol = S}) ->
  io:format("[PORT $] @expecting_var model_end~n"),
  gen_fsm:reply(From, S),
  {next_state, model_received, Data#fsm_state{from = null, sol = []}};
%% A symbolic variable of the solution
%% expecting_var --> expecting_value
handle_info({Port, {data, Bin}}, expecting_var, Data=#fsm_state{port = Port}) ->
  try cuter_json:json_to_term(Bin) of
    Var ->
      io:format("[PORT $] @expecting_var VAR: ~p~n", [Var]),
      {next_state, expecting_value, Data#fsm_state{var = Var}}
  catch _:_ ->
    io:format("[PORT] @expecting_var ~p~n", [Bin]),
    {next_state, expecting_var, Data}
  end;
%% The value of a symbolic variable of the solution
%% expecting_value --> expecting_var
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
%% The port has closed normally
%% Stop the FSM
handle_info({'EXIT', Port, normal}, finished, Data=#fsm_state{port = Port}) ->
  io:format("[PORT] @finished exited~n"),
  {stop, normal, Data#fsm_state{port = null}};
%% Unknown message from the port
handle_info({Port, {data, Bin}}, State, Data=#fsm_state{port = Port}) ->
  io:format("[PORT] @~p ~p~n", [State, Bin]),
  {next_state, State, Data};
%% Unknown message
handle_info(Info, _StateName, Data) ->
  {stop, {unexpected_info, Info}, Data}.


%% ----------------------------------------------------------------------------
%% FSM states
%% ----------------------------------------------------------------------------

%%
%% idle
%%

-spec idle(any(), fsm_state()) -> err_async().
idle(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec idle(any(), from(), fsm_state()) -> {reply, ok, waiting, fsm_state()} | err_sync().
%% Open a port by executing an external program
idle({exec, Command}, _From, Data) ->
  Port = open_port({spawn, Command}, [{packet, 2}, binary, hide]),
  io:format("[FSM] Started FSM ~p~n", [Port]),
  {reply, ok, python_started, Data#fsm_state{port = Port}};
idle(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% python_started
%%

-spec python_started(any(), fsm_state()) -> err_async().
python_started(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec python_started(any(), from(), fsm_state()) -> {reply, ok, trace_loaded, fsm_state()} | err_sync().
%% Send a trace file to the solver and load the generated axioms to a list
python_started({load_trace_file, FileInfo}, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(load_trace_file, FileInfo),
  io:format("[FSM] load_trace_file~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, trace_loaded, Data};
python_started(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% trace_loaded
%%

-spec trace_loaded(any(), fsm_state()) -> err_async().
trace_loaded(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec trace_loaded(any(), from(), fsm_state()) -> {reply, ok, axioms_added, fsm_state()} | err_sync().
%% Add the loaded axioms from the trace file to the solver
trace_loaded(add_axioms, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(add_axioms, nil),
  io:format("[FSM] add_axioms~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, axioms_added, Data};
trace_loaded(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% axioms_added
%%

-spec axioms_added(any(), fsm_state()) -> err_async().
axioms_added(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec axioms_added(any(), from(), fsm_state()) -> {next_state, solving, fsm_state()} | err_sync().
%% Query the solver for the satisfiability of the model
axioms_added(check_model, From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(solve, nil),
  io:format("[FSM] check_model~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {next_state, solving, Data#fsm_state{from = From}};
%% Fix a symbolic variable to a specific value
axioms_added({fix_variable, Mapping}, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(fix_variable, Mapping),
  io:format("[FSM] fix_variable~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, axioms_added, Data};
axioms_added(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% failed
%%

-spec failed(any(), fsm_state()) -> err_async().
failed(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec failed(any(), from(), fsm_state()) -> {reply, ok, finished, fsm_state()} | err_sync().
%% Reset the solver by unloading all the axioms
failed(reset_solver, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(reset_solver, nil),
  io:format("[FSM] reset_solver~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, trace_loaded, Data};
%% Terminate the solver
failed(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(stop, nil),
  io:format("[FSM] stop_exec~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, finished, Data};
failed(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% solved
%%

-spec solved(any(), fsm_state()) -> err_async().
solved(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec solved(any(), from(), fsm_state()) -> {next_state, generating_model, fsm_state()} | err_sync().
%% Request the solution from the solver
solved(get_model, From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(get_model, nil),
  io:format("[FSM] get_model~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {next_state, generating_model, Data#fsm_state{from = From}};
solved(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% generating_model
%%
-spec generating_model(any(), fsm_state()) -> err_async().
generating_model(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec generating_model(any(), from(), fsm_state()) -> err_sync().
generating_model(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% expecting_var
%%

-spec expecting_var(any(), fsm_state()) -> err_async().
expecting_var(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec expecting_var(any(), from(), fsm_state()) -> err_sync().
expecting_var(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% expecting_value
%%

-spec expecting_value(any(), fsm_state()) -> err_async().
expecting_value(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec expecting_value(any(), from(), fsm_state()) -> err_sync().
expecting_value(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% model_received
%%

-spec model_received(any(), fsm_state()) -> err_async().
model_received(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec model_received(any(), from(), fsm_state()) -> {reply, ok, finished, fsm_state()} | err_sync().
%% Stop the solver
model_received(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_json:encode_port_command(stop, nil),
  io:format("[FSM] stop_exec~n~p~n", [Cmd]),
  Port ! {self(), {command, Cmd}},
  {reply, ok, finished, Data};
model_received(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.

%%
%% finished
%%

-spec finished(any(), fsm_state()) -> err_async().
finished(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec finished(any(), from(), fsm_state()) -> err_sync().
finished(Event, _From, Data) ->
  {stop, {unexpected_event, Event}, ok, Data}.


