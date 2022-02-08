-module(cuter_strategy).

-export_type([next_tag_response/0, state/0]).

%% The persistent state type is the union of the states from all available strategies.
-type state() :: cuter_bfs_strategy:state().

-type next_tag_response() :: {ok, cuter_scheduler:operation_id(), cuter_scheduler:handle()} | empty.

%% This is the behaviour that all the strategies should implement.

%% Initializes an optional persistent state that can be used by the strategy algorithm.
-callback init() -> state().

%% Cleans up the optional persistent state.
-callback clean_up(state()) -> ok.

%% Fetches the next node that should be reversed.
-callback locate_next_reversible(state(), cuter_cerl:visited_tags()) -> next_tag_response().

%% Returns true if there are no more nodes to be reversed.
-callback no_paths(state()) -> boolean().

%% Consumes the result of an execution and retuns the updated state.
-callback handle_execution(state(), cuter_analyzer:reversible_with_tags(), cuter_scheduler:handle(), 
                           cuter_cerl:visited_tags(), cuter_scheduler:operation_id(), cuter:depth()) -> state().
