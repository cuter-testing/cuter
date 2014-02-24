%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_tests_lib).

-include("eunit_config.hrl").

-export([setup_dir/0, cleanup_dir/1]).

%% Create a directory for temporary use
-spec setup_dir() -> nonempty_string().
setup_dir() ->
  ok = filelib:ensure_dir(?TMP_DIR),
  ?TMP_DIR.

%% Delete a directory and its contents
-spec cleanup_dir(file:name_all()) -> ok.
cleanup_dir(D) ->
  case filelib:is_regular(D) of
    true  -> file:delete(D);
    false ->
      case file:del_dir(D) of
        ok -> ok;
        {error, eexist} ->
          {ok, Fs} = file:list_dir(D),
          AFs = lists:map(fun(X) -> D ++ "/" ++ X end, Fs),
          lists:foreach(fun cleanup_dir/1, AFs),
          file:del_dir(D);
        _ -> ok
      end
  end.
