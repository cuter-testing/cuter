-module(conc).
-compile([export_all]).

start(Mod, Fun, Args)
  when is_atom(Mod); is_list(Mod) ->
  
    %% Start Code and Trace Servers
    {ok, CodeServer} = gen_server:start_link(conc_cserver, [{limited, [foo]}], []),  % Start code server
    {ok, TraceServer} = gen_server:start_link(conc_tserver, [self()], []),           % Start trace server
    
    StartArgs = [Mod, Fun, Args, concrete, external, CodeServer, TraceServer, {true, self()}],
    spawn(conc_eval, eval, StartArgs),
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

