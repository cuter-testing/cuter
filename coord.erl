-module(coord).
-compile(export_all).

-define(PROFILING_FLAG, false).

run(M, F, As) ->
  process_flag(trap_exit, true),
  CoreDir = "core_temp",
  
  Start = now(),
  profiling_start(?PROFILING_FLAG),
  
  Concolic = conc:init_concolic(M, F, As, CoreDir),
  receive
    {'EXIT', Concolic, Why} ->
      R = {error, Why};
    {Concolic, Results} ->
      R = Results
  end,
  
  profiling_stop(?PROFILING_FLAG),
  End  = now(),
  Time = timer:now_diff(End, Start),
  io:format("Time elapsed = ~w secs~n", [Time/1000000]),
  
  analyze(R).
  
run(B, {X, Y}) ->
  Conf = [{number_of_cores, Y}],
  Args = B:bench_args(X, Conf),
  lists:map(fun(A) -> run(B, run, [A, foo, bar]) end, Args).
  
%% Version :: short | intermediate | long
run_tests() ->
  Version = 'short',
  Cores = erlang:system_info(schedulers_online),
  Conf = [{number_of_cores, Cores}],
  Benchmarks = [bang, genstress, big, ehb, ets_test, mbrot, parallel, pcmark, serialmsg, timer_wheel],
  RunOne = 
    fun(Bench) ->
      io:format("===> Running ~w (~w) ...~n", [Bench, Version]),
      Args = Bench:bench_args(Version, Conf),
      lists:map(fun(A) -> run(Bench, run, [A, foo, bar]) end, Args)
    end,
  lists:map(RunOne, Benchmarks).
  
profiling_start(true) ->
  eprof:start(),
  eprof:start_profiling([self()]);
profiling_start(false) ->
  ok.
  
profiling_stop(true) ->
  eprof:stop_profiling(),
  eprof:analyze(),
  eprof:stop();
profiling_stop(false) ->
  ok.
  
  
analyze({error, Error}) ->
  io:format("ConcServer error : ~p~n", [Error]);
analyze({internal_codeserver_error, Node, Results}) ->
  io:format("Internal CodeServer Error in node ~p~n", [Node]),
  report(Results);
analyze({internal_traceserver_error, Node, Results}) ->
  io:format("Internal TraceServer Error in node ~p~n", [Node]),
  report(Results);
analyze({runtime_error, Node, Results}) ->
  io:format("Runtime error in Node ~p~n", [Node]),
  report(Results);
analyze({ok, _Node, Results}) ->
  report(Results).
  
  
report(R) ->
  L = orddict:to_list(R),
  lists:foreach(fun report_node/1, L).
  
report_node({N, R}) ->
  io:format("Node ~w~n", [N]),
  lists:foreach(fun report_result/1, R).
  
report_result({result, {CR, SR}}) ->
  io:format("Concrete Result = ~p~n", [CR]),
  io:format("Symbolic Result = ~p~n", [SR]);
report_result({mapping, R}) ->
  io:format("Mapping = ~p~n", [R]);
report_result({runtime_error, {_Node, Who, {CErr, SErr}}}) ->
  io:format("Runtime Error in ~p~n", [Who]),
  io:format("Concrete Error = ~p~n", [CErr]),
  io:format("Symbolic Error = ~p~n", [SErr]);
report_result({clogs, Logs}) ->
  io:format("Loaded ~w Modules: ~w~n", [length(Logs), Logs]);
report_result({tlogs, Logs}) ->
  io:format("Monitored Processes : ~w~n", [proplists:get_value(procs, Logs)]);
report_result({codeserver_error, Error}) ->
  io:format("CodeServer Error = ~p~n", [Error]);
report_result({traceserver_error, Error}) ->
  io:format("TraceServer Error = ~p~n", [Error]).

