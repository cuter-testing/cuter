%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_lib).

-include("cuter_macros.hrl").

%% external exports
-export([unique_string/0, ensure_port_or_pid/1, clear_and_delete_dir/1, list_dir/1,
         logfile_name/2]).

%% Generate a unique string
-spec unique_string() -> nonempty_string().
unique_string() -> erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>".

%% Ensure that we use the actual port/pid and not the registered name
-spec ensure_port_or_pid(pid() | port() | atom() | {atom(), atom()}) -> port() | pid().
ensure_port_or_pid(What) when is_pid(What); is_port(What) ->
  What;
ensure_port_or_pid(What) when is_atom(What) ->
  whereis(What);
ensure_port_or_pid({RegName, Node}) when is_atom(RegName), is_atom(Node) ->
  rpc:call(Node, erlang, whereis, [RegName]).

-spec logfile_name(string(), pid()) -> file:filename_all().
logfile_name(Dir, Pid) ->
  F = erlang:pid_to_list(Pid) -- "<>",
  filename:absname(Dir ++ "/proc-" ++ F).

%% Delete the trace files / folders of an execution
-spec clear_and_delete_dir(string()) -> ok.
clear_and_delete_dir(D) ->
  {ok, CWD} = file:get_cwd(),
  clear_dir(CWD ++ "/" ++ D).

clear_dir(D) ->
  case filelib:is_regular(D) of
    true  -> delete_file(D);
    false ->
      case file:del_dir(D) of
        ok -> ok;
        {error, eexist} ->
          {ok, L} = file:list_dir(D),
          LL = [D ++ "/" ++ X || X <- L],
          lists:foreach(fun clear_dir/1, LL),
          file:del_dir(D);
        _ -> ok
      end
  end.

-ifdef(DELETE_TRACE).
delete_file(F) -> file:delete(F).
-else.
delete_file(_) -> ok.
-endif.

%% List the absolute names of the files in a directory in ascending order
-spec list_dir(string()) -> [file:filename()].
list_dir(Dir) ->
  {ok, Fs} = file:list_dir(Dir),
  [Dir ++ "/" ++ F || F <- lists:sort(fun erlang:'<'/2, Fs)].
