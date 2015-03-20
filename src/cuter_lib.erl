%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_lib).

-include("include/cuter_macros.hrl").

%% external exports
-export([get_tmp_dir/1, get_data_dir/2, get_trace_dir/1, get_core_dir/1,
         get_merged_tracefile/1, get_codeserver_dir/1, get_monitor_dir/1, logfile_name/2,
         clear_and_delete_dir/1, clear_and_delete_dir/2, list_dir/1, unique_string/0,
         ensure_port_or_pid/1]).


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

%%====================================================================
%% Manage Directories
%%====================================================================

%% The working directory used during the execution of the tool
%% (relative to a base directory that is provided)
-spec get_tmp_dir(file:filename_all()) -> file:filename_all().
get_tmp_dir(BaseDir) ->
  filename:absname(?RELATIVE_TMP_DIR, BaseDir).

%% The directory to store the data of the specific execution
-spec get_data_dir(file:filename_all(), integer()) -> file:filename_all().
get_data_dir(BaseDir, No) ->
  filename:absname("exec" ++ integer_to_list(No), BaseDir).

%% The directory for all the trace files
-spec get_trace_dir(file:filename_all()) -> file:filename_all().
get_trace_dir(BaseDir) ->
  filename:absname("traces", BaseDir).

%% The directory for the trace files for the processes of a specific monitor server
-spec get_monitor_dir(file:filename_all()) -> file:filename_all().
get_monitor_dir(BaseDir) ->
  U = cuter_lib:unique_string(),
  filename:absname("trace-" ++ U, BaseDir).

%% The directory for all the Core Erlang files
-spec get_core_dir(file:filename_all()) -> file:filename_all().
get_core_dir(BaseDir) ->
  filename:absname("core", BaseDir).

%% The directory for the Core Erlang files of a specific code server
-spec get_codeserver_dir(file:filename_all()) -> file:filename_all().
get_codeserver_dir(BaseDir) ->
  U = cuter_lib:unique_string(),
  filename:absname("codeserver-" ++ U, BaseDir).

%% The file that will hold the merged traces of an execution
-spec get_merged_tracefile(file:filename_all()) -> file:filename_all().
get_merged_tracefile(BaseDir) ->
  filename:absname("run.trace", BaseDir).

%% The file that holds the trace of a specific process
-spec logfile_name(file:filename_all(), pid()) -> file:filename_all().
logfile_name(Dir, Pid) ->
  F = erlang:pid_to_list(Pid) -- "<>",
  filename:absname("proc-" ++ F, Dir).

%% Delete the whole subfolder/file structure of a specific directory
-spec clear_and_delete_dir(file:filename_all()) -> ok.
clear_and_delete_dir(D) ->
  clear_and_delete_dir(D, none).

-spec clear_and_delete_dir(file:filename_all(), file:filename_all() | none) -> ok.
clear_and_delete_dir(F, F) ->
  cuter_pp:delete_file(F, false),
  ok;
clear_and_delete_dir(D, EF) ->
  cuter_pp:delete_file(D, true),
  case filelib:is_regular(D) of
    true ->
      delete_file(D),
      ok;
    false ->
      case file:del_dir(D) of
        ok -> ok;
        {error, eexist} ->
          Fs = list_dir(D),
          lists:foreach(fun(F) -> clear_and_delete_dir(F, EF) end, Fs),
          _ = file:del_dir(D),
          ok;
        _ -> ok
      end
  end.

-ifdef(DELETE_TRACE).
delete_file(F) -> file:delete(F).
-else.
delete_file(_) -> ok.
-endif.

%% List the absolute names of the files/folders in a directory in ascending order
-spec list_dir(file:filename_all()) -> [file:filename_all()].
list_dir(Dir) ->
  {ok, Fs} = file:list_dir(Dir),
  [filename:absname(F, Dir) || F <- lists:sort(fun erlang:'<'/2, Fs)].

