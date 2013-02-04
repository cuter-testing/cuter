%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(coord).

-export([run/3, run_bencherl_demos/0, run_my_demos/0]).

%-define(PROFILING_FLAG, false).
%-define(PRINT_TRACE_FLAG, true).   %% Displays Traces after the execution
%-define(DELETE_TRACE_FLAG, true).  %% Deletes the Traces after the execution

%% -----------------------------------------------------------------------------
%% Concolic Execution of M,F,As
%% -----------------------------------------------------------------------------

-spec run(atom(), atom(), [term()]) -> 'ok'.
run(M, F, As) ->
  process_flag(trap_exit, true),
  CoreDir = "core_temp",
  TraceDir = "traces",
  
  Start = now(),
  %% profiling_start(?PROFILING_FLAG),
  
  Concolic = conc:init_concolic(M, F, As, CoreDir, TraceDir),
  receive
    {'EXIT', Concolic, Why} ->
      R = {error, Why};
    {Concolic, Results} ->
      R = Results
  end,
  
  %% profiling_stop(?PROFILING_FLAG),
  End  = now(),
  Time = timer:now_diff(End, Start),
  io:format("%% Time elapsed = ~w secs~n", [Time/1000000]),
  
  analyze(R),
  Traces = trace_dir(R),
  lists:foreach(fun clear_dir/1, Traces),
  %% Directory will only be deleted if it's empty
  _ = file:del_dir(filename:absname(TraceDir)),
  ok.
  
%% -----------------------------------------------------------------------------
%% Run demos
%% -----------------------------------------------------------------------------
  
%% Bencherl Demos
-spec run_bencherl_demos() -> 'ok'.
run_bencherl_demos() ->
  Version = 'short',  %% Version :: short | intermediate | long
  %%  Cores = erlang:system_info(schedulers_online),
  Cores = 2,
  Conf = [{number_of_cores, Cores}],
  Benchmarks = [bang, genstress, big, ehb, ets_test, mbrot, parallel, pcmark, serialmsg, timer_wheel, ran],
%  Benchmarks = [bang, genstress, big, ehb, ets_test, mbrot, parallel, pcmark, serialmsg, timer_wheel],
%  Benchmarks = [bang],
  RunOne = 
    fun(Bench) ->
      io:format("~n===> Simulating ~w (~w, ~w) ...~n", [Bench, Version, Conf]),
      Args = Bench:bench_args(Version, Conf),
      lists:map(fun(A) -> run(Bench, run, [A, foo, bar]) end, Args)
    end,
  lists:foreach(RunOne, Benchmarks).
  
%% My Demos
-spec run_my_demos() -> 'ok'.
run_my_demos() ->
  Demos = [{fib, [4]}, {min, [[5,1,3]]}, {spawn_apply, [erlang,'++',[[1,2,3],[a,b,c]]]}],
  F = fun({F, As}) ->
    io:format("~n===> Simulating apply(demo, ~w, ~w) ...~n", [F, As]),
    run(demo, F, As)
  end,
  lists:foreach(F, Demos).
  
%% -----------------------------------------------------------------------------
%% Profiling functions
%% -----------------------------------------------------------------------------
%profiling_start(true) ->
%  eprof:start(),
%  eprof:start_profiling([self()]);
%profiling_start(false) ->
%  ok.
%  
%profiling_stop(true) ->
%  eprof:stop_profiling(),
%  eprof:analyze(),
%  eprof:stop();
%profiling_stop(false) ->
%  ok.
  
%% -----------------------------------------------------------------------------
%% Report Results
%% -----------------------------------------------------------------------------
analyze({error, Error}) ->
  io:format("%%   ConcServer error : ~p~n", [Error]);
analyze({internal_codeserver_error, Node, Results}) ->
  io:format("%%   Internal CodeServer Error in node ~p~n", [Node]),
  report(Results);
analyze({internal_traceserver_error, Node, Results}) ->
  io:format("%%   Internal TraceServer Error in node ~p~n", [Node]),
  report(Results);
analyze({runtime_error, Node, Results}) ->
  io:format("%%   Runtime error in Node ~p~n", [Node]),
  report(Results);
analyze({ok, _Node, Results}) ->
  report(Results).
  
report(R) ->
  L = orddict:to_list(R),
  lists:foreach(fun report_node/1, L).
  
report_node({N, R}) ->
  io:format("%% Node ~w~n", [N]),
  lists:foreach(fun report_result/1, R).
  
report_result({result, {CR, SR}}) ->
  io:format("%%   Concrete Result = ~p~n", [CR]),
  io:format("%%   Symbolic Result = ~p~n", [SR]);
report_result({mapping, R}) ->
  io:format("%%   Mapping = ~p~n", [R]);
report_result({runtime_error, {_Node, Who, {CErr, SErr}}}) ->
  io:format("%%   Runtime Error in ~p~n", [Who]),
  io:format("%%   Concrete Error = ~p~n", [CErr]),
  io:format("%%   Symbolic Error = ~p~n", [SErr]);
report_result({clogs, Logs}) ->
  io:format("%%   Loaded ~w Modules: ~w~n", [length(Logs), Logs]);
report_result({tlogs, Logs}) ->
  io:format("%%   Monitored Processes : ~w~n", [proplists:get_value(procs, Logs)]),
  io:format("%%   Traces Directory : ~p~n", [proplists:get_value(dir, Logs)]);
report_result({codeserver_error, Error}) ->
  io:format("%%   CodeServer Error = ~p~n", [Error]);
report_result({traceserver_error, Error}) ->
  io:format("%%   TraceServer Error = ~p~n", [Error]);
report_result(X) ->
  io:format("Unexpected ~w~n", [X]).

%% -----------------------------------------------------------------------------

trace_dir({_Status, _Node, Results}) ->
  Ns = orddict:to_list(Results),
  Rs = lists:map(fun({_N, R}) -> R end, Ns),
  Logs = proplists:get_all_values(tlogs, lists:flatten(Rs)),
  [proplists:get_value(dir, L) || L <- Logs];
trace_dir({error, _Error}) ->
  [].
  
%% temporary deleting all traces
clear_dir(D) ->
  case filelib:is_regular(D) of
    true ->
      {ok, F} = conc_encdec:open_file(D),
      print_trace(F, D),
      conc_encdec:close_file(F),
      delete_trace(D);
    false ->
      case file:del_dir(D) of
        ok -> ok;
        {error, eexist} ->
          {ok, L} = file:list_dir(D),
          LL = lists:map(fun(X) -> D ++ "/" ++ X end, L),
          lists:foreach(fun clear_dir/1, LL),
          file:del_dir(D);
        _ -> ok
      end
  end.

%print_trace(F, D) ->
%  io:format("%% Contents of ~p~n", [D]),
%  conc_encdec:pprint(F).
print_trace(_F, _D) ->
  ok.

delete_trace(F) ->
  file:delete(F).
%  ok.
