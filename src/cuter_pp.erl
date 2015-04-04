%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_pp).

-include("include/cuter_macros.hrl").

-export([ mfa/3
        , input/1
        , exec_status/1
        , exec_info/1
        , path_vertex/1
        , reversible_operations/1
        %% Verbose reporting
        , error_retrieving_spec/2
        %% Verbose Scheduling
        , seed_execution/2
        , request_input/2
        , request_input_empty/1
        , request_input_success/4
        , store_execution/4
        , store_execution_fail/3
        , store_execution_success/4
        , dequeued_handle/1
        , attempting_to_reverse/2
        , solving_failed/0
        , solving_succeeded/1
        , requeue_success/4
        , requeue_failure/4
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

-spec mfa(atom(), atom(), integer()) -> ok.
mfa(M, F, Ar) -> io:format("Testing ~p:~p/~p ...~n", [M, F, Ar]).

-spec input([any()]) -> ok.
input(As) when is_list(As) ->
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

-spec seed_execution(reference(), map()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
seed_execution(Rf, Info) ->
  io:format("SEED EXECUTION~n"),
  io:format("  HANDLE~n"),
  io:format("    ~p~n", [Rf]),
  io:format("  STORED INFO~n"),
  io:format("    ~p~n", [Info]).
-else.
seed_execution(_Rf, _Info) -> ok.
-endif.


-spec request_input(queue:queue(), ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
request_input(Q, I) ->
  io:format("REQUEST INPUT~n"),
  io:format("  CURRENT QUEUE~n"),
  io:format("    ~p~n", [queue:to_list(Q)]),
  io:format("  ALL HANDLES~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  io:format("    ~p~n", [Handles]).
-else.
request_input(_Q, _I) -> ok.
-endif.

-spec request_input_empty(ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
request_input_empty(I) ->
  io:format("UNABLE TO GENERATE A TESTCASE~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  case Handles of
    [] -> ok;
    _ ->
      io:format("  [!!] HANDLES LEFT~n"),
      io:format("    ~p~n", [Handles])
  end.
-else.
request_input_empty(_I) -> ok.
-endif.

-spec request_input_success(reference(), [any()], queue:queue(), ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
request_input_success(Rf, Inp, Q, I) ->
  io:format("NEW INPUT SENT~n"),
  io:format("  HANDLE~n"),
  io:format("    ~p~n", [Rf]),
  io:format("  ARGS~n"),
  io:format("    ~p~n", [Inp]),
  io:format("  NEW QUEUE~n"),
  io:format("    ~p~n", [queue:to_list(Q)]),
  io:format("  ALL HANDLES~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  io:format("    ~p~n", [Handles]).
-else.
request_input_success(_Rf, _Inp, _Q, _I) -> ok.
-endif.

-spec store_execution(reference(), map(), queue:queue(), ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
store_execution(Rf, Info, Q, I) ->
  io:format("STORE EXECUTION~n"),
  io:format("  HANDLE~n"),
  io:format("    ~p~n", [Rf]),
  io:format("  EXEC INFO~n"),
  io:format("    ~p~n", [Info]),
  io:format("  CURRENT QUEUE~n"),
  io:format("    ~p~n", [queue:to_list(Q)]),
  io:format("  ALL HANDLES~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  io:format("    ~p~n", [Handles]).
-else.
store_execution(_Rf, _Info, _Q, _I) -> ok.
-endif.

-spec store_execution_fail(integer(), integer(), ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
store_execution_fail(N, Depth, I) ->
  io:format("WILL NOT STORE EXECUTION~n"),
  io:format("  NEXT REVERSIBLE COMMAND > DEPTH~n"),
  io:format("    ~p > ~p~n", [N, Depth]),
  io:format("  HANDLES LEFT~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  io:format("    ~p~n", [Handles]).
-else.
store_execution_fail(_N, _D, _I) -> ok.
-endif.

-spec store_execution_success(integer(), integer(), queue:queue(), ets:tid()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
store_execution_success(N, Depth, Q, I) ->
  io:format("EXECUTION STORED~n"),
  io:format("  NEXT REVERSIBLE COMMAND =< DEPTH~n"),
  io:format("    ~p =< ~p~n", [N, Depth]),
  io:format("  NEW QUEUE~n"),
  io:format("    ~p~n", [queue:to_list(Q)]),
  io:format("  ALL HANDLES~n"),
  {Handles, _} = lists:unzip(ets:tab2list(I)),
  io:format("    ~p~n", [Handles]).
-else.
store_execution_success(_N, _D, _Q, _I) -> ok.
-endif.

-spec dequeued_handle(reference()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
dequeued_handle(Ref) ->
  io:format("DEQUEUED HANDLE~n"),
  io:format("  ~p~n", [Ref]).
-else.
dequeued_handle(_Ref) -> ok.
-endif.

-spec attempting_to_reverse(integer(), file:name()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
attempting_to_reverse(N, Fname) ->
  io:format("ATTEMPTING TO REVERSE~n"),
  io:format("  OPERATION ~w IN FILE ~p~n", [N, Fname]).
-else.
attempting_to_reverse(_N, _Fname) -> ok.
-endif.

-spec solving_failed() -> ok.
-ifdef(VERBOSE_SCHEDULER).
solving_failed() ->
  io:format("SOLVING FAILED~n").
-else.
solving_failed() ->
  io:format(".~n").
-endif.

-spec solving_succeeded(any()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
solving_succeeded(Inp) ->
  io:format("TESTCASE GENERATED~n"),
  io:format("  ~p~n", [Inp]).
-else.
solving_succeeded(_Inp) -> ok.
-endif.

-spec requeue_success(reference(), integer(), integer(), integer()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
requeue_success(Ref, X, L, D) ->
  io:format("REQUEUING HANDLE~n"),
  io:format("  ~p (~w =< ~w /\\ ~w =< ~w)~n", [Ref, X, L, X, D]).

-else.
requeue_success(_Ref, _X, _L, _D) -> ok.
-endif.

-spec requeue_failure(reference(), integer(), integer(), integer()) -> ok.
-ifdef(VERBOSE_SCHEDULER).
requeue_failure(Ref, X, L, D) ->
  io:format("WILL NOT REQUEUE HANDLE~n"),
  io:format("  ~p (~w > ~w \\/ ~w > ~w)~n", [Ref, X, L, X, D]).
-else.
requeue_failure(_Ref, _X, _L, _D) -> ok.
-endif.

-spec report_erroneous([list(any())]) -> ok.
report_erroneous([]) ->
  divider("-"),
  io:format("NO RUNTIME ERRORS OCCURED~n");
report_erroneous(Err) ->
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

%%
%% Verbose Reporting
%%

%% Reports an error while retrieving the spec of an MFA.
-spec error_retrieving_spec(mfa(), any()) -> ok.
-ifdef(VERBOSE_REPORTING).
error_retrieving_spec(MFA, Error) -> io:format("[SPEC] Spec for ~p not found.~n  Error: ~p~n", [MFA, Error]).
-else.
error_retrieving_spec(MFA, _Error) -> io:format("[SPEC] Spec for ~p not found.~n", [MFA]).
-endif.
