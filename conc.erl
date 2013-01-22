-module(conc).
-export([run/0, run/3, run_tests/0]).

-define(STOP_SERVER(Server), gen_server:call(Server, terminate)).
-define(PROFILING_FLAG, false).

run() ->
  B = timer_wheel,
  Cores = erlang:system_info(schedulers_online),
  Conf = [{number_of_cores, Cores}],
  Args = B:bench_args(short, Conf),
  lists:map(fun(A) -> conc:run(B, run, [A, foo, bar]) end, Args).

run(M, F, As) ->
  
  %% Temporary directory to store Core Erlang code
  CoreDir = filename:absname("core_temp"),
  %% Start Code and Trace Servers
  CodeServer = conc_cserver:init_codeserver(CoreDir),
  TraceServer = conc_tserver:init_traceserver(),
  
  Start = now(),
  profiling_start(?PROFILING_FLAG),
 
  %% Concrete Evaluation of MFA and Execution Logs
  Result = conc_eval:i(M, F, As, CodeServer, TraceServer),
  receive
    {TraceServer, TLogs} -> 
      ?STOP_SERVER(CodeServer)
  end,
  receive
    {CodeServer, CLogs} ->
      ok
  end,
  
  profiling_stop(?PROFILING_FLAG),
  End  = now(),
  Time = timer:now_diff(End, Start),
  
  %% Print Results
%  io:format("---------------------~n"),
%  io:format("Execution Information~n"),
%  io:format("---------------------~n"),
  report_result(Result),
%  report_tlogs(TLogs),
%  report_clogs(CLogs),
%  io:format("Time elapsed = ~w secs~n", [Time/1000000]),
  ok.

  
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
  
report_result({ok, {Mapping, {R, SymbR}}}) ->
  io:format("%% Mapping = ~p~n", [Mapping]),
  io:format("%% Concrete Result = ~p~n", [R]),
  io:format("%% Symbolic Result = ~p~n", [SymbR]);
report_result({error, {Mapping, {Who, CErr, SErr}}}) ->
  io:format("%% Mapping = ~p~n", [Mapping]),
  io:format("%% Runtime Error in ~p~n", [Who]),
  io:format("%% Concrete Reason = ~p~n", [CErr]),
  io:format("%% Semantic Reason = ~p~n", [SErr]).
  
report_tlogs(Logs) ->
  io:format("%% Monitored Processes : ~w~n", [proplists:get_value(procs, Logs)]).
  
report_clogs(Logs) ->
  io:format("%% Loaded ~w Modules: ~w~n", [length(Logs), Logs]).
  
%% Version :: short | intermediate | long
run_tests() ->
  Version = 'short',
  Cores = erlang:system_info(schedulers_online),
  Conf = [{number_of_cores, Cores}],
  Benchmarks = [bang, genstress, big, ehb, ets_test, mbrot, parallel, pcmark, serialmsg, timer_wheel, ran],
  RunOne = 
    fun(Bench) ->
      io:format("===> Running ~w (~w) ...~n", [Bench, Version]),
      Args = Bench:bench_args(Version, Conf),
      lists:map(fun(A) -> conc:run(Bench, run, [A, foo, bar]) end, Args)
    end,
  lists:map(RunOne, Benchmarks).
  
