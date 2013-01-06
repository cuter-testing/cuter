-module(conc).
-compile([export_all]).

start(Mod, Fun, Args)
  when is_atom(Mod); is_list(Mod) ->
  
    %% Start Code and Trace Servers
    Dir = filename:absname("core"),
    {ok, CodeServer} = gen_server:start_link(conc_cserver, [{limited, [foo, rush, rectangle, circle, polymorph]}, Dir], []),  % Start code server
    {ok, TraceServer} = gen_server:start_link(conc_tserver, [self()], []),           % Start trace server
    
    StartArgs = [{named, {Mod, Fun}}, Args, concrete, external, CodeServer, TraceServer, self()],
%    spawn(conc_eval, eval, StartArgs),
    spawn(?MODULE, chk, [self(), StartArgs]),
    receive
      {Time, Val} ->
        io:format("Time = ~w secs~nResult = ~p~n", [Time/1000000, Val])
    end,
%    io:format("[conc]: Spawned ~p~n", [P]),
%    receive
%      B ->
%        io:format("[conc]: Concrete Eval of ~p:~p(~p) =~n~p~n", [Mod, Fun, Args, B])
%    end,

    receive
      {TraceServer, Msg} ->
        gen_server:call(CodeServer, terminate),
        {ok, Msg}
    end;


start(_, _, _) ->
  {error, invalid_module_name}.
  
chk(S, As) ->
%  eprof:start(),
%  eprof:start_profiling([self()]),
  Start = now(),
  Val = apply(conc_eval, eval, As),
  End  = now(),
%  eprof:stop_profiling(),
%  eprof:analyze(),
%  eprof:stop(),
  Time = timer:now_diff(End, Start),
  S ! {Time, Val}.

