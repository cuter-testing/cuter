-module(cuter_strategy_behaviour).
-export([behaviour_info/1]).

%% This is the behaviour that all the strategies should implement which are:
%%
%% init_data/0: Initializes the data structures needed for the strategy algorithm to work
%% destroy_data/1: Accepts the data initialized by init_data and destroys them
%% locate_next_reversible/2: Accepts the strategy data and the visited_tags to fetch
%%                           the next node that needs to be reversed
%% no_paths/1: Accepts the strategy data as input to determine whether there are more
%%             nodes to be reversed
%% handle_execution/6: Accepts the strategy data and the data created by an input run
%%                     and updates the strategy data and returns them

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
  [{init_data, 0}, {destroy_data, 1}, {locate_next_reversible, 2}, {no_paths, 1}, {handle_execution, 6}].
