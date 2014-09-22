%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_lib).

%% external exports
-export([unique_string/0, ensure_port_or_pid/1]).

-spec unique_string() -> nonempty_string().
unique_string() -> erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>".

-spec ensure_port_or_pid(pid() | port() | atom() | {atom(), atom()}) -> port() | pid().
ensure_port_or_pid(What) when is_pid(What); is_port(What) ->
  What;
ensure_port_or_pid(What) when is_atom(What) ->
  whereis(What);
ensure_port_or_pid({RegName, Node}) when is_atom(RegName), is_atom(Node) ->
  rpc:call(Node, erlang, whereis, [RegName]).
