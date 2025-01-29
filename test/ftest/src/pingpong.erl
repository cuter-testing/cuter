-module(pingpong).
-export([start/1, ping/2, pong/0]).

-spec start(integer()) -> ok.
start(N) ->
    PongPid = spawn(?MODULE, pong, []),
    ping(PongPid, N).

ping(PongPid, N) ->
    PongPid ! {ping, self(), N},
    receive
        {pong, RN} ->
            case RN of
                42 -> throw({error, simulated_exception});
                _ -> ok
            end
    end,
    ok.

pong() ->
    receive
        {ping, SenderPid, N} ->
            SenderPid ! {pong, N + 1}
    end.
