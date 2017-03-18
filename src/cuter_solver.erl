%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver).
-behaviour(gen_fsm).

-include("include/cuter_macros.hrl").

-export([
  %% external exports
    lookup_in_model/2
  , start/1
  , poll/2
  , send_stop_message/1
  , solve/1
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
  , model_received/2
  , model_received/3
  , finished/2
  , finished/3
  , failed/2
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
-type err_sync()  :: {stop, {unexpected_event, any()}, ok, fsm_state()}.
-type model()     :: #{cuter_symbolic:symbolic() => any()}.
-type simplifier_conf() :: bitstring().

%% fsm state datatype
-record(fsm_state, {
  super         :: pid(),
  from = null   :: from() | null,
  port = null   :: port() | null,
  sol = #{}     :: model(),
  var = null    :: cuter_symbolic:symbolic() | null,
  debug         :: solver_input()
}).
-type fsm_state() :: #fsm_state{}.

-type mappings() :: [cuter_symbolic:mapping()].
-type solver_input() :: {string(), mappings(), file:name(), cuter_scheduler_maxcover:operationId()}.
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
      case cuter_scheduler_maxcover:request_operation(Scheduler) of
        %% No operation is currently available.
        try_later ->
          timer:sleep(?SLEEP),
          loop(Parent, Scheduler);
        %% Got an operation to solve.
        {Python, Mappings, File, N} ->
          Result = solve({Python, Mappings, File, N}),
          ok = cuter_scheduler_maxcover:solver_reply(Scheduler, Result),
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
  Setting = initial_setting(length(Mappings)),
  query_solver(FSM, Mappings, Setting).

-spec query_solver(pid(), mappings(), bitstring()) -> solver_result().
query_solver(FSM, Mappings, CurrSetting) ->
  ok = add_axioms(FSM),
  load_setting(CurrSetting, Mappings, FSM),
  case check_model(FSM) of
    'SAT' ->
      get_solution(FSM, Mappings);
    'UNSAT' ->
      stop_fsm(FSM, error);  %% RETURN ERROR
    _ ->
      NextSetting = generate_next_setting(CurrSetting, length(Mappings)),
      query_with_new_setting(NextSetting, FSM, Mappings)
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

-spec query_with_new_setting(error, pid(), mappings()) -> error
                          ; ({ok, simplifier_conf()}, pid(), mappings()) -> solver_result().
query_with_new_setting(error, FSM, _Mappings) ->
  stop_fsm(FSM, error);
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
-spec initial_setting(non_neg_integer()) -> simplifier_conf().
initial_setting(N) -> <<0:N>>.

%% Generate the next setting if possible.
-spec generate_next_setting(simplifier_conf(), non_neg_integer()) -> {ok, simplifier_conf()} | error.
generate_next_setting(Setting, N) ->
  Next = next_setting(Setting, N),
  case is_limit_setting(Next, N) of
    true  -> error;
    false -> {ok, Next}
  end.

%% Actually generate the next setting.
%% The current strategy is to have at most one variable fixed,
%% so we just apply a left bit shift to the setting.
-spec next_setting(simplifier_conf(), non_neg_integer()) -> simplifier_conf().
next_setting(Setting, N) ->
  case Setting of
    <<0:N>> -> <<1:N>>;
    <<X:N>> -> <<(X bsl 1):N >>
  end.

%% Check if we have generated all the available settings.
%% For our current strategy, the end is when we have performed
%% N left bit shifts, where N is the length of the bitstring.
-spec is_limit_setting(simplifier_conf(), non_neg_integer()) -> boolean().
is_limit_setting(Setting, N) ->
  Max = << <<1:1>> || _ <- lists:seq(1,N) >>,
  Overflow = <<0:N>>,
  case Setting of
    Max -> true;
    Overflow -> true;
    _ -> false
  end.

%% Load the current setting to the solver.
-spec load_setting(simplifier_conf(), mappings(), pid()) -> ok.
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
-spec start_fsm(solver_input()) -> pid() | {error, term()}.
start_fsm(Args) ->
  case gen_fsm:start_link(?MODULE, [self(), Args], []) of
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
-spec check_model(pid()) -> solver_status().
check_model(Pid) ->
  gen_fsm:sync_send_event(Pid, check_model, 500000).

%% Get the instance of the sat model
-spec get_model(pid()) -> mappings().
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
-spec init([pid()| solver_input(), ...]) -> {ok, idle, fsm_state()}.
init([Super, Debug]) ->
  process_flag(trap_exit, true),
  {ok, idle, #fsm_state{super = Super, debug = Debug}}.

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
%% The solver did not manage to satisfy the model
%% solving --> failed
handle_info({Port, {data, Resp}}, State=solving, Data=#fsm_state{from = From, port = Port}) ->
  try cuter_serial:from_solver_response(Resp) of
    {'status', Status} ->
      pp_solver_status(Status),
      gen_fsm:reply(From, Status),
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
      gen_fsm:reply(From, Model),
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

-spec idle(any(), fsm_state()) -> err_async().
idle(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec idle(any(), from(), fsm_state()) -> {reply, ok, python_started, fsm_state()} | err_sync().
%% Open a port by executing an external program
idle({exec, Command}, _From, Data) ->
  Port = open_port({spawn, Command}, [{packet, 4}, binary, hide]),
  cuter_pp:fsm_started(Port),
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
  Cmd = cuter_serial:solver_command(load_trace_file, FileInfo),
  cuter_pp:send_cmd(python_started, FileInfo, "Load Trace File"),
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
  Cmd = cuter_serial:solver_command(add_axioms),
  cuter_pp:send_cmd(trace_loaded, Cmd, "Load axioms"),
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
  Cmd = cuter_serial:solver_command(solve),
  cuter_pp:send_cmd(axioms_added, Cmd, "Check the model"),
  Port ! {self(), {command, Cmd}},
  {next_state, solving, Data#fsm_state{from = From}};
%% Fix a symbolic variable to a specific value
axioms_added({fix_variable, Mapping}, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(fix_variable, Mapping),
  cuter_pp:send_cmd(axioms_added, Mapping, "Fix a variable"),
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
  Cmd = cuter_serial:solver_command(reset_solver),
  cuter_pp:send_cmd(failed, Cmd, "Reset the solver"),
  Port ! {self(), {command, Cmd}},
  {reply, ok, trace_loaded, Data};
%% Terminate the solver
failed(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(stop),
  cuter_pp:send_cmd(failed, Cmd, "Stop the execution"),
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
  Cmd = cuter_serial:solver_command(get_model),
  cuter_pp:send_cmd(solved, Cmd, "Get the model"),
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
%% model_received
%%

-spec model_received(any(), fsm_state()) -> err_async().
model_received(Event, Data) ->
  {stop, {unexpected_event, Event}, Data}.

-spec model_received(any(), from(), fsm_state()) -> {reply, ok, finished, fsm_state()} | err_sync().
%% Stop the solver
model_received(stop_exec, _From, Data=#fsm_state{port = Port}) ->
  Cmd = cuter_serial:solver_command(stop),
  cuter_pp:send_cmd(model_received, Cmd, "Stop the execution"),
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


%% Dispatcher for pretty printing the status of the solver.
pp_solver_status('SAT') -> ok;
pp_solver_status('UNSAT') -> cuter_pp:solving_failed_unsat();
pp_solver_status('TIMEOUT') -> cuter_pp:solving_failed_timeout();
pp_solver_status('UNKNOWN') -> cuter_pp:solving_failed_unknown().
