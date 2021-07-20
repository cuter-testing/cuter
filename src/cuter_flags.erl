%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_flags).
-export([report_metrics/0]).

-type flag_name() :: atom().

-spec get_boolean_flag(flag_name()) -> boolean().
get_boolean_flag(Name) ->
  case init:get_argument(Name) of
    %% We just want to see if the flag is set; we ignore the actual value.
    {ok, _} -> true;
    error -> false
  end.

%% Returns true if we should report the collected metrics.
-spec report_metrics() -> boolean().
report_metrics() -> get_boolean_flag('report-metrics').
