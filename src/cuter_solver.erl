%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver).
-behaviour(gen_statem).

-include("include/cuter_macros.hrl").
-include("include/cuter_metrics.hrl").

-export([
  %% external exports
    lookup_in_model/2
  , start/1
  , poll/2
  , send_stop_message/1
  , solve/1
  %% gen_fsm callbacks
  , callback_mode/0
  , init/1
  , terminate/3
  , code_change/4
  %% FSM states
  , idle/3
  , python_started/3
  , trace_loaded/3
  , axioms_added/3
  , solved/3
  , solving/3
  , generating_model/3
  , model_received/3
  , finished/3
  , failed/3
]).

-export_type([model/0, state/0, solver_input/0, solver_result/0, solver_status/0]).

-define(SLEEP, 100).

-type solver_status() :: 'SAT' | 'UNSAT' | 'UNKNOWN' | 'TIMEOUT'.

-type state()     :: idle
                   | python_started
                   | trace_loaded
                   | axioms_added
                   | solving
                   | failed
                   | solved
                   | generating_model
                   | model_received
                   | finished.
-type from()      :: {pid(), reference()}.
-type err_async() :: {stop, {unexpected_event, any()}, fsm_state()}.
-type err_sync()  :: {stop_and_reply, {unexpected_event, any()}, {reply, from(), ok}, fsm_state()}.
-type ok_reply()  :: {reply, from(), ok}.
-type model()     :: #{cuter_symbolic:symbolic() => any()}.

%% fsm state datatype
-record(fsm_state, {
  from = null   :: from() | null,
  port = null   :: port() | null,
  sol = #{}     :: model(),
  var = null    :: cuter_symbolic:symbolic() | null,
  debug         :: solver_input()
}).
-type fsm_state() :: #fsm_state{}.

-type mappings() :: [cuter_symbolic:mapping()].
-type solver_input() :: {string(), mappings(), file:name(), cuter_scheduler:operation_id()}.
-type solver_result() :: {ok, cuter:input()} | error.

%% ----------------------------------------------------------------------------
%% Start a Solver process
%% ----------------------------------------------------------------------------

%% Starts a Solver process.
-spec start(pid()) -> pid().
start(Scheduler) ->
  spawn_link(?MODULE, poll, [self(), Scheduler]).

%% Initializes and starts the loop.
-spec poll(pid(), pid()) -> ok.
poll(Parent, Scheduler) ->
  process_flag(trap_exit, true),
  loop(Parent, Scheduler).

%% Enters the loop where it will query the scheduler for an operation to
%% reverse and then report the result.
-spec loop(pid(), pid()) -> ok.
loop(Parent, Scheduler) ->
  case got_stop_message(Parent) of
    true -> stop();
    false ->
      %% Query the scheduler.
      case cuter_scheduler:request_operation(Scheduler) of
        %% No operation is currently available.
        try_later ->
          timer:sleep(?SLEEP),
          loop(Parent, Scheduler);
        %% Got an operation to solve.
        {Python, Mappings, File, N} ->
          Result = solve({Python, Mappings, File, N}),
          ok = cuter_scheduler:solver_reply(Scheduler, Result),
          loop(Parent, Scheduler)
      end
  end.

%% Stops a Solver process.
-spec send_stop_message(pid()) -> ok.
send_stop_message(Solver) ->
  Solver ! {self(), stop},
  ok.

%% Checks if the solver process should stop.
-spec got_stop_message(pid()) -> boolean().
got_stop_message(Parent) ->
  receive {Parent, stop} -> true
  after 0 -> false
  end.

%% Cleans up before exiting.
-spec stop() -> ok.
stop() ->
  ok.

%% ----------------------------------------------------------------------------
%% Query the Z3 SMT Solver
%% ----------------------------------------------------------------------------

-spec solve(solver_input()) -> solver_result().
solve({Python, Mappings, File, N}=Args) ->
  FSM = start_fsm(Args),
  ok = exec(FSM, Python),
  ok = load_trace_file(FSM, {File, N}),
  query_solver(FSM, Mappings).

-spec query_solver(pid(), mappings()) -> solver_result().
query_solver(FSM, Mappings) ->
  ok = add_axioms(FSM),
  Status = check_model(FSM),
  cuter_metrics:measure_distribution(?SOLVER_STATUS_METRIC, Status),
  case Status of
    'SAT' ->
      get_solution(FSM, Mappings);
    _ ->
      stop_fsm(FSM, error)  %% RETURN ERROR
  end.

-spec get_solution(pid(), mappings()) -> solver_result().
get_solution(FSM, Mappings) ->
  M = get_model(FSM),
  ok = stop_exec(FSM),
  Inp = cuter_symbolic:generate_new_input(Mappings, M),
  wait_for_fsm(FSM, {ok, Inp}).

stop_fsm(FSM, Ret) ->
  ok = stop_exec(FSM),
  wait_for_fsm(FSM, Ret).

-spec wait_for_fsm(pid(), solver_result()) -> solver_result().
wait_for_fsm(FSM, Ret) ->
  receive {'EXIT', FSM, normal} -> Ret
  after 10 ->
%%    io:format("TIMEOUT~n"),
    Ret
  end.

%% Lookup the value of a symbolic var in the generated model
-spec lookup_in_model(cuter_symbolic:symbolic(), model()) -> any().
lookup_in_model(Var, Model) ->
  maps:get(Var, Model).

%% ----------------------------------------------------------------------------
%% API to interact with the FSM
%% ----------------------------------------------------------------------------

%% Start the FSM
-spec start_fsm(solver_input()) -> pid() | {error, term()}.
start_fsm(Args) ->
  case gen_statem:start_link(?MODULE, [Args], []) of
    {ok, Pid} -> Pid;
    {error, _Reason} = R -> R
  end.

%% Execute an external program
%% In this case, it will be a Python program.
-spec exec(pid(), string()) -> ok.
exec(Pid, Python) ->
  gen_statem:call(Pid, {exec, Python}).

%% Load the trace file
-spec load_trace_file(pid(), {file:name(), integer()}) -> ok.
load_trace_file(Pid, FileInfo) ->
  gen_statem:call(Pid, {load_trace_file, FileInfo}, 10000).

%% Add the generated axioms to the solver
-spec add_axioms(pid()) -> ok.
add_axioms(Pid) ->
  gen_statem:call(Pid, add_axioms).

%% Check the model for satisfiability
-spec check_model(pid()) -> solver_status().
check_model(Pid) ->
  gen_statem:call(Pid, check_model, 500000).

%% Get the instance of the sat model
-spec get_model(pid()) -> mappings().
get_model(Pid) ->
  gen_statem:call(Pid, get_model, 500000).

%% Stop the FSM
-spec stop_exec(pid()) -> ok.
stop_exec(Pid) ->
  gen_statem:call(Pid, stop_exec).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

%% ----------------------------------------------------------------------------
%% gen_fsm callbacks
%% ----------------------------------------------------------------------------

%% init/1
-spec init([solver_input(), ...]) -> {ok, idle, fsm_state()}.
init([Debug]) ->
  process_flag(trap_exit, true),
  {ok, idle, #fsm_state{ debug = Debug }}.

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

%% handle_info/3
%% Useful to handle messages from the port
-spec handle_info({port(), {data, binary()}}, state(), fsm_state()) -> {next_state, state(), fsm_state()}
                                                                     | {stop, {unexpected_info, any()}, fsm_state()}.

%% The model is satisfiable and the solver has generated a solution
%% solving --> solved
%% The solver did not manage to satisfy the model
%% solving --> failed
handle_info({Port, {data, Resp}}, State=solving, Data=#fsm_state{from = From, port = Port}) ->
  try cuter_serial:from_solver_response(Resp) of
    {'status', Status} ->
      pp_solver_status(Status),
      gen_statem:reply(From, Status),
      case Status of
        'SAT' ->
          {next_state, solved, Data#fsm_state{from = null}};
        _ ->
          {next_state, failed, Data#fsm_state{from = null}}
      end;
    {'model', Model} ->
      {stop, {expecting_status_got_model, Model}, Data}
  catch
    _:_ ->
      cuter_pp:undecoded_msg(Resp, State),
      {next_state, State, Data}
  end;
%% Delimit the start of the solution
%% generating_model --> model_received
handle_info({Port, {data, Resp}}, State=generating_model, Data=#fsm_state{from = From, port = Port}) ->
  try cuter_serial:from_solver_response(Resp) of
    {'model', Model} ->
      Fn = fun(Var, Val, _) ->
          cuter_pp:received_var(Var),
          cuter_pp:received_val(Val),
          ok
        end,
      maps:fold(Fn, ok, Model),
      gen_statem:reply(From, Model),
      {next_state, model_received, Data#fsm_state{from = null, sol = #{}}};
    {'status', Status} ->
      {stop, {expecting_model_got_status, Status}, Data}
  catch
    _:_ ->
      cuter_pp:undecoded_msg(Resp, State),
      {next_state, State, Data}
  end;
%% The port has closed normally
%% Stop the FSM
handle_info({'EXIT', Port, normal}, finished, Data=#fsm_state{port = Port}) ->
  cuter_pp:port_closed(),
  {stop, normal, Data#fsm_state{port = null}};
%% Unknown message from the port
handle_info({Port, {data, Bin}}, State, Data=#fsm_state{port = Port}) ->
  cuter_pp:undecoded_msg(Bin, State),
  {next_state, State, Data};
%% Unknown message
handle_info(Info, _State, Data) ->
  cuter_pp:debug_unexpected_solver_message(Data#fsm_state.debug),
  {stop, {unexpected_info, Info}, Data}.


%% ----------------------------------------------------------------------------
%% FSM states
%% ----------------------------------------------------------------------------

%%
%% idle
%%

-spec idle(cast, any(), fsm_state()) -> err_async();
          ({call, from()}, any(), fsm_state()) -> {next_state, python_started, fsm_state(), [ok_reply()]} | err_sync();
          (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

idle(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Open a port by executing an external program
idle({call, From}, {exec, Command}, Data) ->
  Port = open_port({spawn, Command}, [{packet, 4}, binary, hide]),
  cuter_pp:fsm_started(Port),
  {next_state, python_started, Data#fsm_state{port = Port}, [{reply, From, ok}]};
idle({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
idle(info, Msg, Data) ->
  handle_info(Msg, idle, Data).

%%
%% python_started
%%

-spec python_started(cast, any(), fsm_state()) -> err_async();
                    ({call, from()}, any(), fsm_state()) -> {next_state, trace_loaded, fsm_state(), [ok_reply()]} | err_sync();
                    (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

python_started(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Send a trace file to the solver and load the generated axioms to a list
python_started({call, From}, {load_trace_file, FileInfo}, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(load_trace_file, FileInfo),
  cuter_pp:send_cmd(python_started, FileInfo, "Load Trace File"),
  Port ! {self(), {command, Cmd}},
  {next_state, trace_loaded, Data, [{reply, From, ok}]};
python_started({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
python_started(info, Msg, Data) ->
  handle_info(Msg, python_started, Data).

%%
%% trace_loaded
%%

-spec trace_loaded(cast, any(), fsm_state()) -> err_async();
                  ({call, from()}, any(), fsm_state()) -> {next_state, axioms_added, fsm_state(), [ok_reply()]} | err_sync();
                  (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

trace_loaded(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Add the loaded axioms from the trace file to the solver
trace_loaded({call, From}, add_axioms, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(add_axioms),
  cuter_pp:send_cmd(trace_loaded, Cmd, "Load axioms"),
  Port ! {self(), {command, Cmd}},
  {next_state, axioms_added, Data, [{reply, From, ok}]};
trace_loaded({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
trace_loaded(info, Msg, Data) ->
  handle_info(Msg, trace_loaded, Data).

%%
%% axioms_added
%%

-spec axioms_added(cast, any(), fsm_state()) -> err_async();
                  ({call, from()}, any(), fsm_state()) -> {next_state, solving, fsm_state()} | err_sync();
                  (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

axioms_added(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Query the solver for the satisfiability of the model
axioms_added({call, From}, check_model, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(solve),
  cuter_pp:send_cmd(axioms_added, Cmd, "Check the model"),
  Port ! {self(), {command, Cmd}},
  {next_state, solving, Data#fsm_state{from = From}};
axioms_added({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
axioms_added(info, Msg, Data) ->
  handle_info(Msg, axioms_added, Data).
  
%%
%% failed
%%

-spec failed(cast, any(), fsm_state()) -> err_async();
            ({call, from()}, any(), fsm_state()) -> {next_state, trace_loaded | finished, fsm_state(), [ok_reply()]} | err_sync();
            (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

failed(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Terminate the solver
failed({call, From}, stop_exec, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(stop),
  cuter_pp:send_cmd(failed, Cmd, "Stop the execution"),
  Port ! {self(), {command, Cmd}},
  {next_state, finished, Data, [{reply, From, ok}]};
failed({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
failed(info, Msg, Data) ->
  handle_info(Msg, failed, Data).

%%
%% solving
%%

-spec solving(cast, any(), fsm_state()) -> err_async();
             ({call, from()}, any(), fsm_state()) -> err_sync();
             (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

solving(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Request the solution from the solver
solving({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
solving(info, Msg, Data) ->
  handle_info(Msg, solving, Data).

%%
%% solved
%%

-spec solved(cast, any(), fsm_state()) -> err_async();
            ({call, from()}, any(), fsm_state()) -> {next_state, generating_model, fsm_state()} | err_sync();
            (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

solved(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Request the solution from the solver
solved({call, From}, get_model, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(get_model),
  cuter_pp:send_cmd(solved, Cmd, "Get the model"),
  Port ! {self(), {command, Cmd}},
  {next_state, generating_model, Data#fsm_state{from = From}};
solved({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
solved(info, Msg, Data) ->
  handle_info(Msg, solved, Data).

%%
%% generating_model
%%

-spec generating_model(cast, any(), fsm_state()) -> err_async();
                      ({call, from()}, any(), fsm_state()) -> err_sync();
                      (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

generating_model(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
generating_model({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
generating_model(info, Msg, Data) ->
  handle_info(Msg, generating_model, Data).

%%
%% model_received
%%

-spec model_received(cast, any(), fsm_state()) -> err_async();
                    ({call, from()}, any(), fsm_state()) -> {next_state, finished, fsm_state(), [ok_reply()]} | err_sync();
                    (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

model_received(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
%% Stop the solver
model_received({call, From}, stop_exec, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(stop),
  cuter_pp:send_cmd(model_received, Cmd, "Stop the execution"),
  Port ! {self(), {command, Cmd}},
  {next_state, finished, Data, [{reply, From, ok}]};
model_received({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
model_received(info, Msg, Data) ->
  handle_info(Msg, model_received, Data).

%%
%% finished
%%

-spec finished(cast, any(), fsm_state()) -> err_async();
              ({call, from()}, any(), fsm_state()) -> err_sync();
              (info, any(), fsm_state()) -> {next_state, state(), fsm_state()} | err_async().

finished(cast, Event, Data) ->
  {stop, {unexpected_event, Event}, Data};
finished({call, From}, Event, Data) ->
  {stop_and_reply, {unexpected_event, Event}, {reply, From, ok}, Data};
finished(info, Msg, Data) ->
  handle_info(Msg, finished, Data).

%% Dispatcher for pretty printing the status of the solver.
pp_solver_status('SAT') -> ok;
pp_solver_status('UNSAT') -> cuter_pp:solving_failed_unsat();
pp_solver_status('TIMEOUT') -> cuter_pp:solving_failed_timeout();
pp_solver_status('UNKNOWN') -> cuter_pp:solving_failed_unknown().
