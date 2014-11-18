%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Simple models to test the semantics of each log entry.
-spec solve_simple_test_() -> term().
solve_simple_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Ts = [ {"No constraints", fun just_params/1}
       , {"Constraints - Guard True", fun guard_true/1}
       , {"Constraints - Guard False", fun guard_false/1}
       , {"Constraints - Match Equal", fun match_equal/1}
       , {"Constraints - Match Not Equal", fun match_not_equal/1}
       , {"Constraints - Nonempty List", fun nonempty_list/1}
       , {"Constraints - Empty List", fun empty_list/1}
       , {"Constraints - Not a List", fun not_a_list/1}
       , {"Constraints - Tuple of Size N", fun tuple_sz/1}
       , {"Constraints - Tuple of Not Size N", fun tuple_not_sz/1}
       , {"Constraints - Not a Tuple", fun not_a_tuple/1}
       , {"Constraints - Unfold a Symbolic Tuple", fun unfold_tuple/1}
       , {"Constraints - Unfold a Symbolic List", fun unfold_list/1}
       , {"BIFs - erlang:hd/1", fun erlang_hd/1}
       , {"BIFs - erlang:tl/1", fun erlang_tl/1}
       , {"BIFs - erlang:is_integer/1", fun erlang_is_integer/1}
       , {"BIFs - erlang:is_atom/1", fun erlang_is_atom/1}
       , {"BIFs - erlang:is_float/1", fun erlang_is_float/1}
       , {"BIFs - erlang:is_list/1", fun erlang_is_list/1}
       , {"BIFs - erlang:is_tuple/1", fun erlang_is_tuple/1}
       , {"BIFs - erlang:is_boolean/1", fun erlang_is_boolean/1}
       , {"BIFs - erlang:is_number/1", fun erlang_is_number/1}
       ],
  [{"Simple Queries: " ++ Desc, {setup, Setup, Cleanup, Inst}} || {Desc, Inst} <- Ts].


-spec setup() -> {file:name(), file:name(), string()}.
setup() ->
  put('__conc_depth', 100),
  Dir = cuter_tests_lib:setup_dir(),
  Fname = filename:absname("logfile", Dir),
  Python = cuter_tests_lib:get_python_command(),
  {Dir, Fname, Python}.

cleanup({Dir, _Fname, _Python}) ->
  cuter_lib:clear_and_delete_dir(Dir).

-spec create_logfile(file:name(), [any()], fun((file:io_device(), [cuter_symbolic:symbolic()]) -> ok)) -> [cuter_symbolic:mapping()].
create_logfile(Fname, As, CreateLogs) ->
  {ok, Fd} = cuter_log:open_file(Fname, write),  % Create the logfile
  {SAs, Mapping} = cuter_symbolic:abstract(As),  % Abstract the input
  ok = CreateLogs(Fd, SAs),                      % Add the logs
  cuter_log:close_file(Fd),                      % Close the logfile
  Mapping.                                       % Return the mapping

%% ----------------------------------------------------------------------------
%% Just define the function parameters.
%% No constraints added.
%% ----------------------------------------------------------------------------

just_params({_Dir, Fname, Python}) ->
  %% Create a random number of random integers to serve as the arguments
  As = [random:uniform(42) || _ <- lists:seq(1, random:uniform(42))],
  Mapping = create_logfile(Fname, As, fun just_params_logs/2),
  {ok, Sol} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"The solution equals the input", ?_assertEqual(As, Sol)}].

just_params_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs).

%% ----------------------------------------------------------------------------
%% The Guard True and Guard False constraints
%% ----------------------------------------------------------------------------

guard_true({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun guard_true_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Guard True", ?_assertEqual(true, SolNormal)}
  , {"Result for Guard True Reversed", ?_assertEqual(false, SolRev)}
  ].

guard_true_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_guard(Fd, true, P1).

guard_false({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun guard_false_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Guard False", ?_assertEqual(false, SolNormal)}
  , {"Result for Guard False Reversed", ?_assertEqual(true, SolRev)}
  ].

guard_false_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_guard(Fd, false, P1).

%% ----------------------------------------------------------------------------
%% The Match Equal and Match Not Equal constraints
%% ----------------------------------------------------------------------------

match_equal({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun match_equal_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Match Equal", ?_assertEqual(ok, SolNormal)}
  , {"Result for Match Equal Reversed", ?_assertNotEqual(ok, SolRev)}
  ].

match_equal_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_equal(Fd, true, P1, ok).

match_not_equal({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun match_not_equal_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Match Not Equal", ?_assertNotEqual(ok, SolNormal)}
  , {"Result for Match Not Equal Reversed", ?_assertEqual(ok, SolRev)}
  ].

match_not_equal_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_equal(Fd, false, P1, ok).

%% ----------------------------------------------------------------------------
%% The NonEmpty List, Empty List and Not a List constraints
%% ----------------------------------------------------------------------------

nonempty_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun nonempty_list_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for NonEmpty List", ?_assertMatch([_|_], SolNormal)}
  , {"Result for NonEmpty List Reversed", ?_assertNotMatch([_|_], SolRev)}
  ].

nonempty_list_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_list(Fd, nonempty, P1).

empty_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun empty_list_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Empty List", ?_assertMatch([], SolNormal)}
  , {"Result for Empty List Reversed", ?_assertNotMatch([], SolRev)}
  ].

empty_list_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_list(Fd, empty, P1).

not_a_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun not_a_list_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Not a List", ?_assertNotMatch(X when is_list(X), SolNormal)}
  , {"Result for Not a List Reversed", ?_assertMatch(X when is_list(X), SolRev)}
  ].

not_a_list_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_list(Fd, not_lst, P1).

%% ----------------------------------------------------------------------------
%% The Tuple of Size N, Tuple of Not Size N and Not a Tuple constraints
%% ----------------------------------------------------------------------------

tuple_sz({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun tuple_sz_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Tuple of Size N", ?_assertMatch({_,_}, SolNormal)}
  , {"Result for Tuple of Size N Reversed", ?_assertNotMatch({_,_}, SolRev)}
  ].

tuple_sz_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_tuple(Fd, sz, P1, 2).

tuple_not_sz({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun tuple_not_sz_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Tuple of Not Size N", ?_assertMatch(X when is_tuple(X) andalso tuple_size(X) =/= 2, SolNormal)}
  , {"Result for Tuple of Not Size N Reversed", ?_assertNotMatch({_,_}, SolRev)}
  ].

tuple_not_sz_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_tuple(Fd, not_sz, P1, 2).

not_a_tuple({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun not_a_tuple_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Result for Not a Tuple", ?_assertNotMatch(X when is_tuple(X), SolNormal)}
  , {"Result for Not a Tuple Reversed", ?_assertMatch(X when is_tuple(X), SolRev)}
  ].

not_a_tuple_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  cuter_log:log_tuple(Fd, not_tpl, P1, 2).

%% ----------------------------------------------------------------------------
%% The auxialiary unfold operations
%% ----------------------------------------------------------------------------

unfold_tuple({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun unfold_tuple_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"Just ensure it's a tuple", ?_assertMatch({_,_,_}, Sol)}].

unfold_tuple_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  Vs = [cuter_symbolic:fresh_symbolic_var() || _ <- lists:seq(1,3)],
  cuter_log:log_unfold_symbolic(Fd, break_tuple, P1, Vs).

unfold_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun unfold_list_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"Just ensure it's a list", ?_assertMatch([_,_,_], Sol)}].

unfold_list_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  Vs = [cuter_symbolic:fresh_symbolic_var() || _ <- lists:seq(1,3)],
  cuter_log:log_unfold_symbolic(Fd, break_list, P1, Vs).

%% ----------------------------------------------------------------------------
%% BIFs
%% ----------------------------------------------------------------------------

%%
%% erlang:hd/1
%%

erlang_hd({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_hd_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"It's a list with the proper head", ?_assertEqual([ok], SolNormal)}
  , {"Make it throw an exception", ?_assertError(badarg, erlang:hd(SolRev))}
  ].

erlang_hd_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, hd, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, ok).

%%
%% erlang:tl/1
%%

erlang_tl({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_tl_logs/2),
  {ok, [SolNormal]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [SolRev]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"It's a list with the proper tail", ?_assertMatch([_], SolNormal)}
  , {"Make it throw an exception", ?_assertError(badarg, erlang:tl(SolRev))}
  ].

erlang_tl_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, tl, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, []).

%%
%% erlang:is_integer/1
%%

erlang_is_integer({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_integer_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's an integer", ?_assertMatch(X when is_integer(X), Sol)}].

erlang_is_integer_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_integer, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_atom/1
%%

erlang_is_atom({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_atom_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's an atom", ?_assertMatch(X when is_atom(X), Sol)}].

erlang_is_atom_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_atom, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_float/1
%%

erlang_is_float({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_float_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a float", ?_assertMatch(X when is_float(X), Sol)}].

erlang_is_float_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_float, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_list/1
%%

erlang_is_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_list_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a list", ?_assertMatch(X when is_list(X), Sol)}].

erlang_is_list_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_list, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_tuple/1
%%

erlang_is_tuple({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_tuple_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a tuple", ?_assertMatch(X when is_tuple(X), Sol)}].

erlang_is_tuple_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_tuple, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_boolean/1
%%

erlang_is_boolean({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_boolean_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a boolean", ?_assertMatch(X when is_boolean(X), Sol)}].

erlang_is_boolean_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_boolean, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).

%%
%% erlang:is_number/1
%%

erlang_is_number({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_number_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a number", ?_assertMatch(X when is_number(X), Sol)}].

erlang_is_number_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_number, 1}, SAs, X),
  cuter_log:log_equal(Fd, true, X, true).
