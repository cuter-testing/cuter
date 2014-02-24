%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_lib).

%% external exports
-export([unique_string/0]).

-spec unique_string() -> nonempty_string().
unique_string() -> erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>".
