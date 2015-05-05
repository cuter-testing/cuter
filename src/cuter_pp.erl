%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_pp).
-behaviour(gen_server).

-include("include/cuter_macros.hrl").

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).
-export([start/0, stop/0]).

%% Report information about a given concolic execution.
-export([mfa/1, input/2, error_retrieving_spec/2, execution_status/2, execution_info/2, path_vertex/2, flush/1]).

-export([ reversible_operations/1
        %% Verbose Scheduling
        , report_erroneous/1
        %% Verbose File/Folder Deletion
        , delete_file/2
        %% Verbose solving
        , sat/0
        , not_sat/0
        , model_start/0
        , model_end/0
        , received_var/1
        , received_val/1
        , port_closed/0
        , undecoded_msg/2
        , fsm_started/1
        , send_cmd/3
        %% Verbose merging
        , file_finished/1
        , set_goal/2
        , consume_msg/1
        , goal_already_achieved/1
        , search_goal_in_file/1
        , change_to_file/1
        , open_file/2
        , achieve_goal/2
        , open_pending_file/1
       ]).

-record(sts, {
  super :: pid(),
  info  :: dict:dict(cuter_scheduler_maxcover:exec_handle(), orddict:orddict())
}).
-type state() :: #sts{}.

%% ============================================================================
%% Eternal API
%% ============================================================================

%% Starts the server.
-spec start() -> ok.
start() ->
  case gen_server:start({local, ?PRETTY_PRINTER}, ?MODULE, [self()], []) of
    {ok, _Pid} -> ok;
    {error, Reason} -> exit({failed_to_start_ppserver, Reason})
  end.

%% Stops the server.
-spec stop() -> ok.
stop() ->
  gen_server:cast(?PRETTY_PRINTER, {stop, self()}).

%% The MFA that will be tested.
-spec mfa(mfa()) -> ok.
mfa(MFA) ->
  gen_server:call(?PRETTY_PRINTER, {mfa, MFA}).

%% The input that will be tested.
-spec input(cuter_scheduler_maxcover:exec_handle(), cuter:input()) -> ok.
input(Ref, As) ->
  gen_server:call(?PRETTY_PRINTER, {input, Ref, As}).

%% Error while retrieving the spec for the MFA.
-spec error_retrieving_spec(mfa(), any()) -> ok.
error_retrieving_spec(MFA, Error) ->
  gen_server:call(?PRETTY_PRINTER, {error_retrieving_spec, MFA, Error}).

%% The result status of the given concolic execution.
-spec execution_status(cuter_scheduler_maxcover:exec_handle(), cuter_iserver:execution_status()) -> ok.
execution_status(Ref, Status) ->
  gen_server:call(?PRETTY_PRINTER, {execution_status, Ref, Status}).

%% The information of the given concolic execution.
-spec execution_info(cuter_scheduler_maxcover:exec_handle(), orddict:orddict()) -> ok.
execution_info(Ref, Info) ->
  gen_server:call(?PRETTY_PRINTER, {execution_info, Ref, Info}).

%% The path vertex of the given concolic execution.
-spec path_vertex(cuter_scheduler_maxcover:exec_handle(), cuter_analyzer:path_vertex()) -> ok.
path_vertex(Ref, Vertex) ->
  gen_server:call(?PRETTY_PRINTER, {path_vertex, Ref, Vertex}).

%% Display the information of the given concolic execution.
-spec flush(cuter_scheduler_maxcover:exec_handle()) -> ok.
flush(Ref) ->
  gen_server:call(?PRETTY_PRINTER, {flush, Ref}).

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([pid(), ...]) -> {ok, state()}.
init([Super]) ->
%  process_flag(trap_exit, true),
  link(Super),
  {ok, #sts{super = Super, info = dict:new()}}.

%% gen_server callback : terminate/2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call(any(), {pid(), reference()}, state()) -> {reply, ok, state()}.


%% A new input to be tested.
handle_call({input, Ref, Input}, _From, State=#sts{info = Info}) ->
  Data = orddict:store(input, Input, orddict:new()),
  {reply, ok, State#sts{info = dict:store(Ref, Data, Info)}};
%% The MFA to be tested.
handle_call({mfa, {M, F, Ar}}, _From, State) ->
  io:format("Testing ~p:~p/~p ...~n", [M, F, Ar]),
  {reply, ok, State};
%% The status of the given concolic execution.
handle_call({execution_status, Ref, Status}, _From, State=#sts{info = Info}) ->
  Update = fun(Curr) -> orddict:store(execution_status, Status, Curr) end,
  {reply, ok, State#sts{info = dict:update(Ref, Update, Info)}};
%% Error retrieving the spec of the MFA.
handle_call({error_retrieving_spec, MFA, _Error}, _From, State) ->
  case get(spec_error) of
    yes -> {reply, ok, State};
    undefined ->
      put(spec_error, yes),
      io:format("WARNING: ~p does not have a spec or it is not currently supported!~n", [MFA]),
      {reply, ok, State}
  end;
%% The information of the given concolic execution.
handle_call({execution_info, Ref, ExecInfo}, _From, State=#sts{info = Info}) ->
  Update = fun(Curr) -> orddict:store(execution_info, ExecInfo, Curr) end,
  {reply, ok, State#sts{info = dict:update(Ref, Update, Info)}};
%% The path vertex of the given concolic execution.
handle_call({path_vertex, Ref, Vertex}, _From, State=#sts{info = Info}) ->
  Update = fun(Curr) -> orddict:store(path_vertex, Vertex, Curr) end,
  {reply, ok, State#sts{info = dict:update(Ref, Update, Info)}};
%% Display the information of the given concolic execution.
handle_call({flush, Ref}, _From, State=#sts{info = Info}) ->
  print_execution_info(dict:fetch(Ref, Info)),
  {reply, ok, State#sts{info = dict:erase(Ref, Info)}}.


%% gen_server callback : handle_cast/2
-spec handle_cast(any(), state()) -> {stop, normal, state()} | {noreply, state()}.
%% Stops the server.
handle_cast({stop, FromWho}, State=#sts{super = Super}) ->
  case FromWho =:= Super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end.

%% gen_server callback : handle_info/2
%% Msg when a monitored process exited normally
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_What, State) ->
  {noreply, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec print_execution_info(orddict:orddict()) -> ok.
print_execution_info(Info) ->
  input(orddict:fetch(input, Info)),
  exec_status(orddict:fetch(execution_status, Info)),
  exec_info(orddict:fetch(execution_info, Info)),
  path_vertex(orddict:fetch(path_vertex, Info)).


-spec input([any()]) -> ok.
input(As) when is_list(As) ->
  io:format("~n"),
  divider("="),
  io:format("INPUT~n"),
  input_parameters(As).

input_parameters([]) -> io:format("N/A~n");
input_parameters([H|T]) ->
  io:format("  ~p", [H]),
  lists:foreach(fun(X) -> io:format(", ~p", [X]) end, T),
  io:format("~n").

-spec exec_status(cuter_iserver:execution_status()) -> ok.
-ifdef(VERBOSE_REPORTING).
exec_status({success, {Cv, Sv}}) ->
  io:format("OK~n"),
  io:format("  CONCRETE: ~p~n", [Cv]),
  io:format("  SYMBOLIC: ~p~n", [Sv]);
exec_status({runtime_error, Node, Pid, {Cv, Sv}}) ->
  io:format("RUNTIME ERROR~n"),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  PID: ~p~n", [Pid]),
  io:format("  CONCRETE: ~p~n", [Cv]),
  io:format("  SYMBOLIC: ~p~n", [Sv]);
exec_status({internal_error, Type, Node, Why}) ->
  io:format("INTERNAL ERROR~n"),
  io:format("  TYPE: ~p~n", [Type]),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  REASON: ~p~n", [Why]).
-else.
exec_status({success, {Cv, _Sv}}) ->
  io:format("OK~n"),
  io:format("  CONCRETE: ~p~n", [Cv]);
exec_status({runtime_error, _Node, _Pid, {Cv, _Sv}}) ->
  io:format("RUNTIME ERROR~n"),
  io:format("  CONCRETE: ~p~n", [Cv]);
exec_status({internal_error, Type, Node, Why}) ->
  io:format("INTERNAL ERROR~n"),
  io:format("  TYPE: ~p~n", [Type]),
  io:format("  NODE: ~p~n", [Node]),
  io:format("  REASON: ~p~n", [Why]).
-endif.

-spec exec_info(orddict:orddict()) -> ok.
exec_info(Info) ->
  io:format("EXECUTION INFO~n"),
  F = fun({Node, Data}) ->
    io:format("  ~p~n", [Node]),
    lists:foreach(fun exec_info_data/1, Data)
  end,
  lists:foreach(F, Info).

-spec exec_info_data(any()) -> ok.
-ifdef(VERBOSE_REPORTING).
exec_info_data({mapping, Ms}) ->
  io:format("    MAPPING~n"),
  lists:foreach(fun({Sv, Cv}) -> io:format("      ~p <=> ~p~n", [Sv, Cv]) end, Ms);
exec_info_data({monitor_logs, Logs}) ->
  io:format("    MONITOR LOGS~n"),
  io:format("      ~p~n", [Logs]);
exec_info_data({code_logs, Logs}) ->
  io:format("    CODE LOGS~n"),
  code_logs(Logs);
exec_info_data({int, Pid}) ->
  io:format("    INT~n"),
  io:format("      ~p~n", [Pid]);
exec_info_data(_Data) -> ok.
-else.
exec_info_data({code_logs, Logs}) -> code_logs(Logs);
exec_info_data(_Data) -> ok.
-endif.

-spec code_logs([any()]) -> ok.
-ifdef(VERBOSE_REPORTING).
code_logs([]) -> ok;
code_logs([{loaded_mods, Ms}|Logs]) ->
  io:format("      LOADED MODS~n"),
  io:format("        ~p~n", [Ms]),
  code_logs(Logs);
code_logs([{unsupported_mfas, MFAs}|Logs]) ->
  io:format("      UNSUPPORTED MFAS~n"),
  io:format("        ~p~n", [MFAs]),
  code_logs(Logs);
code_logs([{visited_tags, Tags}|Logs]) ->
  io:format("      VISITED TAGS~n"),
  io:format("        ~p~n", [gb_sets:to_list(Tags)]),
  code_logs(Logs);
code_logs([{stored_mods, Stored}|Logs]) ->
  io:format("      STORED MODS~n"),
  io:format("        ~p~n", [[M || {M, _Info} <- orddict:to_list(Stored)]]),
  code_logs(Logs);
code_logs([{tags_added_no, N}|Logs]) ->
  io:format("      NO OF ADDED TAGS~n"),
  io:format("        ~p~n", [N]),
  code_logs(Logs).
-else.
code_logs([]) -> ok;
code_logs([{unsupported_mfas, MFAs}|Logs]) ->
  io:format("    UNSUPPORTED MFAS: ~p~n", [MFAs]),
  code_logs(Logs);
code_logs([_L|Logs]) -> code_logs(Logs).
-endif.

-spec path_vertex(cuter_analyzer:path_vertex()) -> ok.
path_vertex(Vertex) ->
  io:format("PATH VERTEX~n"),
  io:format("  ~p (~w)~n", [Vertex, length(Vertex)]).

-spec reversible_operations(integer()) -> ok.
reversible_operations(RvsCnt) ->
  io:format("REVERSIBLE OPERATIONS~n"),
  io:format("  ~w~n", [RvsCnt]).

divider(Divider) ->
  lists:foreach(fun(_) -> io:format(Divider) end, lists:seq(1,50)),
  io:format("~n").

%%
%% Verbose Scheduling
%%

-spec report_erroneous(cuter:erroneous_inputs()) -> ok.
report_erroneous([]) ->
  io:format("~n"),
  divider("-"),
  io:format("NO RUNTIME ERRORS OCCURED~n");
report_erroneous(Err) ->
  io:format("~n"),
  divider("-"),
  io:format("INPUTS THAT LEAD TO RUNTIME ERRORS~n"),
  report_erroneous_loop(Err, 1).

report_erroneous_loop([], _N) -> ok;
report_erroneous_loop([I|Is], N) ->
  io:format("[#~w] ", [N]),
  print_input(I),
  report_erroneous_loop(Is, N+1).

print_input([X]) ->
  io:format("~p~n", [X]);
print_input([X, Y | Rest]) ->
  io:format("~p, ", [X]),
  print_input([Y | Rest]).

%%
%% Verbose File/Folder Deletion
%%

-spec delete_file(file:name(), boolean()) -> ok.
-ifdef(VERBOSE_FILE_DELETION).
delete_file(F, true) ->
  io:format("[DELETE] ~p (OK)~n", [F]);
delete_file(F, false) ->
  io:format("[DELETE] ~p (LEAVE INTACT)~n", [F]).
-else.
delete_file(_F, _Intact) -> ok.
-endif.

%%
%% Verbose solving
%%

-spec sat() -> ok.
-ifdef(VERBOSE_SOLVING).
sat() -> io:format("[SLV] (solving) SAT~n").
-else.
sat() -> ok.
-endif.

-spec not_sat() -> ok.
-ifdef(VERBOSE_SOLVING).
not_sat() -> io:format("[SLV] (solving) NOT SAT~n").
-else.
not_sat() -> ok.
-endif.

-spec model_start() -> ok.
-ifdef(VERBOSE_SOLVING).
model_start() -> io:format("[SLV] (generating_model) Beginning of the model~n").
-else.
model_start() -> ok.
-endif.

-spec model_end() -> ok.
-ifdef(VERBOSE_SOLVING).
model_end() -> io:format("[SLV] (expecting_var) End of the model~n").
-else.
model_end() -> ok.
-endif.

-spec received_var(cuter_symbolic:symbolic()) -> ok.
-ifdef(VERBOSE_SOLVING).
received_var(Var) -> io:format("[SLV] (expecting_var) Var: ~p~n", [Var]).
-else.
received_var(_Var) -> ok.
-endif.

-spec received_val(any()) -> ok.
-ifdef(VERBOSE_SOLVING).
received_val(Val) -> io:format("[SLV] (expecting_value) Val: ~p~n", [Val]).
-else.
received_val(_Val) -> ok.
-endif.

-spec port_closed() -> ok.
-ifdef(VERBOSE_SOLVING).
port_closed() -> io:format("[SLV] (finished) Port Closed~n").
-else.
port_closed() -> ok.
-endif.

-spec undecoded_msg(binary(), cuter_solver:state()) -> ok.
-ifdef(VERBOSE_SOLVING).
undecoded_msg(Msg, State) -> io:format("[SLV INFO] (~p) ~p~n", [State, Msg]).
-else.
undecoded_msg(_Msg, _State) -> ok.
-endif.

-spec fsm_started(port()) -> ok.
-ifdef(VERBOSE_SOLVING).
fsm_started(Port) -> io:format("[FSM] (idle) Started (Port ~p)~n", [Port]).
-else.
fsm_started(_Port) -> ok.
-endif.

-spec send_cmd(cuter_solver:state(), binary(), string()) -> ok.
-ifdef(VERBOSE_SOLVING).
send_cmd(State, Cmd, Descr) -> io:format("[FSM] (~p) ~p~n  ~p~n", [State, Descr, Cmd]).
-else.
send_cmd(_State, _Cmd, _Descr) -> ok.
-endif.

%%
%% Verbose Merging
%%

-spec file_finished(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
file_finished(File) -> io:format("[MERGE] Fully parsed ~p~n", [File]).
-else.
file_finished(_File) -> ok.
-endif.

-spec set_goal(cuter_merger:goal(), string()) -> ok.
-ifdef(VERBOSE_MERGING).
set_goal(Goal, Tp) -> io:format("[MERGE] (~p) Set Goal ~p~n", [Tp, Goal]).
-else.
set_goal(_Goal, _Tp) -> ok.
-endif.

-spec consume_msg(reference()) -> ok.
-ifdef(VERBOSE_MERGING).
consume_msg(Rf) -> io:format("[MERGE] (MSG CONSUME) ~p~n", [Rf]).
-else.
consume_msg(_Rf) -> ok.
-endif.

-spec goal_already_achieved(cuter_merger:goal()) -> ok.
-ifdef(VERBOSE_MERGING).
goal_already_achieved(Goal) -> io:format("[MERGE] Already achieved ~p~n", [Goal]).
-else.
goal_already_achieved(_Goal) -> ok.
-endif.

-spec search_goal_in_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
search_goal_in_file(File) -> io:format("[MERGE] Will look in ~p~n", [File]).
-else.
search_goal_in_file(_File) -> ok.
-endif.

-spec change_to_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
change_to_file(File) -> io:format("[MERGE] Changing to ~p~n", [File]).
-else.
change_to_file(_File) -> ok.
-endif.

-spec open_file(file:name(), [file:name()]) -> ok.
-ifdef(VERBOSE_MERGING).
open_file(File, Pending) ->
  io:format("[MERGE] Opening ~p~n", [File]),
  io:format("  Pending files~n"),
  io:format("    ~p~n", [Pending]).
-else.
open_file(_File, _Pending) -> ok.
-endif.

-spec achieve_goal(cuter_merger:goal(), cuter_merger:goal()) -> ok.
-ifdef(VERBOSE_MERGING).
achieve_goal(Goal, NextGoal) ->
  io:format("[MERGE] Achieved ~p~n", [Goal]),
  io:format("  Changing to ~p~n", [NextGoal]).
-else.
achieve_goal(_Goal, _NextGoal) -> ok.
-endif.

-spec open_pending_file(file:name()) -> ok.
-ifdef(VERBOSE_MERGING).
open_pending_file(File) -> io:format("[MERGE] Opening from pending ~p~n", [File]).
-else.
open_pending_file(_File) -> ok.
-endif.
