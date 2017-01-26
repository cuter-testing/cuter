%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_lib).

-include("include/cuter_macros.hrl").

%% external exports
-export([get_tmp_dir/1, get_data_dir/2, get_trace_dir/1,
         get_merged_tracefile/1, get_monitor_dir/1, logfile_name/2,
         clear_and_delete_dir/1, clear_and_delete_dir/2, list_dir/1,
         unique_string/0, ensure_port_or_pid/1, is_improper_list/1,
         get_parts_of_list/1, create_improper_list/2, unzip_with/2]).
-export([mk_lambda/3, is_lambda/1, lambda_arity/1, lambda_kvs/1,
         lambda_default/1, compile_lambda/1, compile_lambdas_in_args/1,
         is_unbound_var/1, handle_unbound_var/1]).

-export_type([lambda/0, lambda_kvs/0, lambda_default/0, lambda_arity/0]).

%% ----------------------------------------------------------------------------
%% Representation of lambda terms.
%% ----------------------------------------------------------------------------

-type lambda_kvs()     :: [{list(), any()}].
-type lambda_default() :: any().
-type lambda_arity()   :: arity().

-define(DEFAULT_ANY_VALUE, 0).

-define(lambda, '__lambda').
-record(?lambda, {
  arity   :: lambda_arity(),
  kvs     :: lambda_kvs(),
  default :: lambda_default()
}).
-type lambda() :: #?lambda{}.


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
-spec get_tmp_dir(file:filename_all()) -> file:filename().
get_tmp_dir(BaseDir) ->
  filename:absname(?RELATIVE_TMP_DIR, BaseDir).

%% The directory to store the data of the specific execution
-spec get_data_dir(file:filename(), cuter_scheduler_maxcover:handle()) -> file:filename().
get_data_dir(BaseDir, Handle) ->
  filename:absname("exec" ++ Handle, BaseDir).

%% The directory for all the trace files
-spec get_trace_dir(file:filename_all()) -> file:filename().
get_trace_dir(BaseDir) ->
  filename:absname("traces", BaseDir).

%% The directory for the trace files for the processes of a specific monitor server
-spec get_monitor_dir(file:filename_all()) -> file:filename().
get_monitor_dir(BaseDir) ->
  U = cuter_lib:unique_string(),
  filename:absname("trace-" ++ U, BaseDir).

%% The file that will hold the merged traces of an execution
-spec get_merged_tracefile(file:filename_all()) -> file:filename().
get_merged_tracefile(BaseDir) ->
  filename:absname("run.trace", BaseDir).

%% The file that holds the trace of a specific process
-spec logfile_name(file:filename_all(), pid()) -> file:filename().
logfile_name(Dir, Pid) ->
  F = erlang:pid_to_list(Pid) -- "<>",
  filename:absname("proc-" ++ F, Dir).

%% Delete the whole subfolder/file structure of a specific directory
-spec clear_and_delete_dir(file:filename_all()) -> ok.
clear_and_delete_dir(D) ->
  clear_and_delete_dir(D, none).

-spec clear_and_delete_dir(file:filename_all(), file:filename() | none) -> ok.
clear_and_delete_dir(F, F) ->
  cuter_pp:delete_file(F, false),
  ok;
clear_and_delete_dir(D, EF) ->
  cuter_pp:delete_file(D, true),
  case filelib:is_regular(D) of
    true ->
      _ = delete_file(D),
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
-spec list_dir(file:filename_all()) -> [file:filename()].
list_dir(Dir) ->
  {ok, Fs} = file:list_dir(Dir),
  [filename:absname(F, Dir) || F <- lists:sort(fun erlang:'<'/2, Fs)].

%% Checks if a list is nil terminated.
-spec is_improper_list(list()) -> boolean().
is_improper_list([]) -> false;
is_improper_list([_|T]) -> is_improper_list(T);
is_improper_list(_) -> true.

-spec get_parts_of_list(list()) -> {list(), any()}.
get_parts_of_list(L) -> get_parts_of_list(L, []).

get_parts_of_list([H|T], Acc) ->
  get_parts_of_list(T, [H|Acc]);
get_parts_of_list(T, Acc) ->
  {lists:reverse(Acc), T}.

-spec create_improper_list(list(), any()) -> list().
create_improper_list(Terms, Acc) ->
  create_improper_list_h(lists:reverse(Terms), Acc).

create_improper_list_h([], Acc) -> Acc;
create_improper_list_h([H|T], Acc) -> create_improper_list_h(T, [H|Acc]).

%% Unzips a list by
%% 1) applying a function to each element of the list that
%%    returns a tuple of two elements.
%% 2) accumulates each element to two new lists.
-spec unzip_with(fun((any()) -> {any(), any()}), list()) -> {list(), list()}.
unzip_with(Fn, L) ->
  unzip_with(Fn, L, [], []).

unzip_with(_Fn, [], L1, L2) ->
  {lists:reverse(L1), lists:reverse(L2)};
unzip_with(Fn, [H|T], L1, L2) ->
  {H1, H2} = Fn(H),
  unzip_with(Fn, T, [H1|L1], [H2|L2]).

%% ----------------------------------------------------------------------------
%% Representation of lambda terms.
%% We need to keep them in a form that allows pretty printing and compilation
%% to actual lambdas when needed.
%% ----------------------------------------------------------------------------

-spec mk_lambda(lambda_kvs(), lambda_default(), lambda_arity()) -> lambda().
mk_lambda(KVs, Default, Arity) ->
  #?lambda{arity = Arity, kvs = KVs, default = Default}.

-spec is_lambda(any()) -> boolean().
is_lambda(#?lambda{arity = Arity, kvs = KVs}) when is_integer(Arity), is_list(KVs) ->
  lists:all(fun({K, _}) when length(K) =:= Arity -> true; (_) -> false end, KVs);
is_lambda(_) -> false.

-spec lambda_arity(lambda()) -> lambda_arity().
lambda_arity(T) ->
  true = is_lambda(T),
  T#?lambda.arity.

-spec lambda_kvs(lambda()) -> lambda_kvs().
lambda_kvs(T) ->
  true = is_lambda(T),
  T#?lambda.kvs.

-spec lambda_default(lambda()) -> lambda_default().
lambda_default(T) ->
  true = is_lambda(T),
  T#?lambda.default.

-spec compile_lambdas_in_args([any()]) -> [any()].
compile_lambdas_in_args(Args) ->
  [ensure_compiled_value(A) || A <- Args].

ensure_compiled_value(V) ->
  case is_lambda(V) of
    true  -> compile_lambda(V);
    false -> V
  end.

-spec compile_lambda(lambda()) -> function().
compile_lambda(T) ->
  true = is_lambda(T),
  compile_lambda_h(T).

compile_lambda_h(#?lambda{arity = Arity, kvs = KVs, default = Default}) ->
  %% TODO Check if the parameters is a lambda, in case of recursion.
  %% TODO Also compile lambdas in maps.
  CompiledKVs = [{K, compile_lambda_h(V)} || {K, V} <- KVs],
  CompiledDefault = compile_lambda_h(Default),
  Dict = dict:from_list(CompiledKVs),
  Lookup = fun(As) -> lookup_args(As, Dict, CompiledDefault) end,
  case Arity of
    0 -> fun() -> Default end;
    1 -> fun(A1) -> Lookup([A1]) end;
    2 -> fun(A1, A2) -> Lookup([A1, A2]) end;
    3 -> fun(A1, A2, A3) -> Lookup([A1, A2, A3]) end;
    4 -> fun(A1, A2, A3, A4) -> Lookup([A1, A2, A3, A4]) end;
    5 -> fun(A1, A2, A3, A4, A5) -> Lookup([A1, A2, A3, A4, A5]) end;
    6 -> fun(A1, A2, A3, A4, A5, A6) -> Lookup([A1, A2, A3, A4, A5, A6]) end;
    7 -> fun(A1, A2, A3, A4, A5, A6, A7) -> Lookup([A1, A2, A3, A4, A5, A6, A7]) end;
    8 -> fun(A1, A2, A3, A4, A5, A6, A7, A8) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8]) end;
    9 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9]) end;
    10 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]) end;
    11 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]) end;
    12 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]) end;
    13 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]) end;
    14 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]) end;
    15 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]) end;
    16 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]) end;
    17 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]) end;
    18 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]) end;
    19 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]) end;
    20 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) ->
      Lookup([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]) end;
    _ -> throw({over_lambda_fun_argument_limit, Arity})
  end;
compile_lambda_h(L) when is_list(L) ->
  [compile_lambda_h(X) || X <- L];
compile_lambda_h(T) when is_tuple(T) ->
  L = tuple_to_list(T),
  L1 = [compile_lambda_h(X) || X <- L],
  list_to_tuple(L1);
compile_lambda_h(T) ->
  handle_unbound_var(T).

-spec handle_unbound_var(any()) -> any().
handle_unbound_var(T) ->
  case is_unbound_var(T) of
    true  -> ?DEFAULT_ANY_VALUE;
    false -> T
  end.

lookup_args(As, Dict, Default) ->
  case dict:find(As, Dict) of
    error -> Default;
    {ok, Value} -> Value
  end.

%% Checks if the supplied term is an expression that denotes ANY term.
-spec is_unbound_var(any()) -> boolean().
is_unbound_var(X) ->
  X =:= ?UNBOUND_VAR_PREFIX.
