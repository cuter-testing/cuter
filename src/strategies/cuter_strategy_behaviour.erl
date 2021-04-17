-module(cuter_strategy_behaviour).
-export([behaviour_info/1]).

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
  [{init_data, 0}, {destroy_data, 1}, {locate_next_reversible, 2}, {no_paths, 1}, {handle_execution, 6}].
