%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_log).

-export([open_file/2]).

-type mode() :: read | write.

%% Opens a file for logging or reading terms
-spec open_file(file:name(), mode()) -> {ok, file:io_device()}.
open_file(F, M) when M =:= read; M =:= write ->
  file:open(F, [M, raw, binary, compressed, {delayed_write, 262144, 2000}]).
