%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_solver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Simple models to test the semantics of each log entry.
-spec solve_simple_test_() -> term().
solve_simple_test_() ->
  Setup = fun setup/0,
  Cleanup = fun cleanup/1,
  Ts = [ { "MFA's Parameters & Spec definitions"
         , [ {"Just define the parameters", fun just_params/1} ]
         }
       , { "Constraints"
         , [ {"Guard True", fun guard_true/1}
           , {"Guard False", fun guard_false/1}
           , {"Match equal", fun match_equal/1}
           , {"Match Not Equal", fun match_not_equal/1}
           , {"Nonempty List", fun nonempty_list/1}
           , {"Empty List", fun empty_list/1}
           , {"Not a List", fun not_a_list/1}
           , {"Tuple of Size N", fun tuple_sz/1}
           , {"Tuple of Not Size N", fun tuple_not_sz/1}
           , {"Not a Tuple", fun not_a_tuple/1}
           ]
         }
       , { "Unfolding symbolic variables"
         , [ {"Unfold a Symbolic Tuple", fun unfold_tuple/1}
           , {"Unfold a Symbolic List", fun unfold_list/1}
           ]
         }
       , { "Bogus operations"
         , [ {"Identity function", fun bogus_identity/1} ]
         }
       , { "Type conversions"
         , [ {"Number to float", fun erlang_float/1}
           , {"List to tuple", fun lst_to_tpl/1}
           , {"Tuple to list", fun tpl_to_lst/1}
           ]
         }
       , { "Query types"
         , [ {"Is integer", fun erlang_is_integer/1}
           , {"Is atom", fun erlang_is_atom/1}
           , {"Is float", fun erlang_is_float/1}
           , {"Is tuple", fun erlang_is_tuple/1}
           , {"Is list", fun erlang_is_list/1}
           , {"Is boolean", fun erlang_is_boolean/1}
           , {"Is number", fun erlang_is_number/1}
           ]
         }
       , { "Arithmetic operations"
         , [ {"Addition", fun erlang_plus/1}
           , {"Subtraction", fun erlang_minus/1}
           , {"Multiplication", fun erlang_times/1}
           , {"Real division", fun erlang_rdiv/1}
           , {"Integer division of natural numbers", fun erlang_posdiv/1}
           , {"Remainder of the integer division of natural numbers", fun erlang_posrem/1}
           , {"Unary operation", fun erlang_unary/1}
           ]
         }
       , { "Operations on atoms" 
         , [ {"Is an empty atom", fun atom_nil/1}
           , {"First letter in an atom", fun atom_head/1}
           , {"An atom without its first letter", fun atom_tail/1}
           ]
         }
       , { "Operations on lists"
         , [ {"Head of a list", fun erlang_hd/1}
           , {"Tail of a list", fun erlang_tl/1}
           ]
         }
       , { "Comparisons"
         , [ {"Equality", fun erlang_equal/1}
           , {"Inequality", fun erlang_unequal/1}
           , {"Compare integers (<)", fun erlang_lt_int/1}
           , {"Compare floats (<)", fun erlang_lt_float/1}
           ]
         }
       ],
  [{Category, [{Dsc, {setup, Setup, Cleanup, I}} || {Dsc, I} <- Xs]} || {Category, Xs} <- Ts].


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
  cuter_log:log_guard(Fd, true, P1, cuter_cerl:empty_tag()).

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
  cuter_log:log_guard(Fd, false, P1, cuter_cerl:empty_tag()).

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
  cuter_log:log_equal(Fd, true, P1, ok, cuter_cerl:empty_tag()).

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
  cuter_log:log_equal(Fd, false, P1, ok, cuter_cerl:empty_tag()).

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
  cuter_log:log_list(Fd, nonempty, P1, cuter_cerl:empty_tag()).

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
  cuter_log:log_list(Fd, empty, P1, cuter_cerl:empty_tag()).

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
  cuter_log:log_list(Fd, not_lst, P1, cuter_cerl:empty_tag()).

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
  cuter_log:log_tuple(Fd, sz, P1, 2, cuter_cerl:empty_tag()).

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
  cuter_log:log_tuple(Fd, not_sz, P1, 2, cuter_cerl:empty_tag()).

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
  cuter_log:log_tuple(Fd, not_tpl, P1, 2, cuter_cerl:empty_tag()).

%% ----------------------------------------------------------------------------
%% The auxiliary unfold operations
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

%% Head of a list.

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
  cuter_log:log_mfa(Fd, {erlang, hd, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, ok, cuter_cerl:empty_tag()).

%% Tail of a list.

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
  cuter_log:log_mfa(Fd, {erlang, tl, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, [], cuter_cerl:empty_tag()).

%% Is an integer.

erlang_is_integer({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_integer_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's an integer", ?_assertMatch(X when is_integer(X), Sol)}].

erlang_is_integer_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_integer, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is an atom.

erlang_is_atom({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_atom_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's an atom", ?_assertMatch(X when is_atom(X), Sol)}].

erlang_is_atom_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_atom, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is a float.

erlang_is_float({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_float_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a float", ?_assertMatch(X when is_float(X), Sol)}].

erlang_is_float_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_float, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is a list.

erlang_is_list({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_list_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a list", ?_assertMatch(X when is_list(X), Sol)}].

erlang_is_list_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_list, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is a tuple.

erlang_is_tuple({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_tuple_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a tuple", ?_assertMatch(X when is_tuple(X), Sol)}].

erlang_is_tuple_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_tuple, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is a boolean.

erlang_is_boolean({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_boolean_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a boolean", ?_assertMatch(X when is_boolean(X), Sol)}].

erlang_is_boolean_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_boolean, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Is a number.

erlang_is_number({_Dir, Fname, Python}) ->
  As = [p1],  % One argument (the type is irrelevant)
  Mapping = create_logfile(Fname, As, fun erlang_is_number_logs/2),
  {ok, [Sol]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [{"It's a number", ?_assertMatch(X when is_number(X), Sol)}].

erlang_is_number_logs(Fd, SAs) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, is_number, 1}, SAs, X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Addition.

erlang_plus({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments (int and float)
  Mapping = create_logfile(Fname, As, fun erlang_plus_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Adding integers", ?_assertEqual(3, P1)}
  , {"Adding integers and floats", ?_assertEqual(0.14, P2)}
  ].

erlang_plus_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_plus, 2}, [P1, 42], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 45, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_plus, 2}, [P1, P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 3.14, cuter_cerl:empty_tag()).

%% Subtraction.

erlang_minus({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments (int and float)
  Mapping = create_logfile(Fname, As, fun erlang_minus_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Subtracting integers", ?_assertEqual(2, P1)}
  , {"Subtracting integers and floats", ?_assertEqual(1.75, P2)}
  ].

erlang_minus_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_minus, 2}, [P1, 42], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, -40, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_minus, 2}, [P1, P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 0.25, cuter_cerl:empty_tag()).

%% Multiplication.

erlang_times({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments (int and float)
  Mapping = create_logfile(Fname, As, fun erlang_times_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Multiplting integers", ?_assertEqual(20, P1)}
  , {"Multiplting integers and floats", ?_assertEqual(2.22, P2)}
  ].

erlang_times_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_times, 2}, [P1, 2], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 40, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_times, 2}, [P1, P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 44.4, cuter_cerl:empty_tag()).

%% Real division.

erlang_rdiv({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments (int and float)
  Mapping = create_logfile(Fname, As, fun erlang_rdiv_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Dividing integers", ?_assertEqual(80.0, P1)}
  , {"Dividing integers and floats", ?_assertEqual(2.5, P2)}
  ].

erlang_rdiv_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_rdiv, 2}, [P1, 2], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 40.0, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, safe_rdiv, 2}, [P1, P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 32.0, cuter_cerl:empty_tag()).

%% Integer division of natural numbers

erlang_posdiv({_Dir, Fname, Python}) ->
  As = [0, 0],  % Two arguments (ints)
  Mapping = create_logfile(Fname, As, fun erlang_posdiv_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV, P2_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Integer division with natural numbers I", ?_assertMatch(X when is_integer(X) andalso X >= 8 andalso X < 12, P1)}
  , {"Integer division with natural numbers II", ?_assertMatch(X when is_integer(X) andalso X div P1 =:= 3, P2)}
  , {"Make it throw an exception", ?_assertError(badarith, P1_RV div P2_RV)}
  ].

erlang_posdiv_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, pos_div, 2}, [P1, 4], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 2, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, pos_div, 2}, [P2, P1], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 3, cuter_cerl:empty_tag()).

%% Remainder of integer division of natural numbers

erlang_posrem({_Dir, Fname, Python}) ->
  As = [0, 0],  % Two arguments (ints)
  Mapping = create_logfile(Fname, As, fun erlang_posrem_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV, P2_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Remainder of integer division with natural numbers I", ?_assertMatch(X when is_integer(X) andalso X rem 4 =:= 2, P1)}
  , {"Remainder of integer division with natural numbers II", ?_assertMatch(X when is_integer(X) andalso X rem P1 =:= 3, P2)}
  , {"Make it throw an exception", ?_assertError(badarith, P1_RV rem P2_RV)}
  ].

erlang_posrem_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, pos_rem, 2}, [P1, 4], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 2, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, pos_rem, 2}, [P2, P1], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 3, cuter_cerl:empty_tag()).

%% Unary operation

erlang_unary({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments (int and float)
  Mapping = create_logfile(Fname, As, fun erlang_unary_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Unary operation on integers", ?_assertEqual(-2, P1)}
  , {"Unary operation on floats", ?_assertEqual(3.14, P2)}
  ].

erlang_unary_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, '-', 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 2, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, '-', 1}, [P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, -3.14, cuter_cerl:empty_tag()).

%% Equality

erlang_equal({_Dir, Fname, Python}) ->
  As = [0, 0.0, 0],  % Two arguments
  Mapping = create_logfile(Fname, As, fun erlang_equal_logs/2),
  {ok, [P1, P2, P3]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Equality of terms I", ?_assertEqual(42, P1)}
  , {"Equality of terms II", ?_assertEqual(ok, P2)}
  , {"Inequality of integers and floats", ?_assertMatch(X when is_integer(X), P3)}
  ].

erlang_equal_logs(Fd, SAs=[P1, P2, P3]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  Z = cuter_symbolic:fresh_symbolic_var(),
  Q = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, '=:=', 2}, [P1, 42], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, '=:=', 2}, [P2, ok], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, true, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, float, 1}, [P3], Z, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, '=:=', 2}, [P3, Z], Q, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Q, false, cuter_cerl:empty_tag()).

%% Inequality

erlang_unequal({_Dir, Fname, Python}) ->
  As = [0, 0.0, 0],  % Two arguments
  Mapping = create_logfile(Fname, As, fun erlang_unequal_logs/2),
  {ok, [P1, P2, P3]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Inequality of terms I", ?_assertEqual(42, P1)}
  , {"Inequality of terms II", ?_assertEqual(ok, P2)}
  , {"Inequality of integers and floats", ?_assertMatch(X when is_integer(X), P3)}
  ].

erlang_unequal_logs(Fd, SAs=[P1, P2, P3]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  Z = cuter_symbolic:fresh_symbolic_var(),
  Q = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, '=/=', 2}, [P1, 42], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, false, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, '=/=', 2}, [P2, ok], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, false, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, float, 1}, [P3], Z, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, '=/=', 2}, [P3, Z], Q, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Q, true, cuter_cerl:empty_tag()).

%% Number to float

erlang_float({_Dir, Fname, Python}) ->
  As = [0, 0.0],  % Two arguments
  Mapping = create_logfile(Fname, As, fun erlang_float_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV, _P2_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Convert integer to float", ?_assertEqual(42, P1)}
  , {"Convert float to float", ?_assertEqual(3.14, P2)}
  , {"Make it throw an exception", ?_assertError(badarg, float(P1_RV))}
  ].

erlang_float_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  Z = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, float, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 42.0, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, float, 1}, [P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, 3.14, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {erlang, is_integer, 1}, [P1], Z, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Z, true, cuter_cerl:empty_tag()).

%% Bogus operation (identity function)

bogus_identity({_Dir, Fname, Python}) ->
  As = [0],
  Mapping = create_logfile(Fname, As, fun bogus_identity_logs/2),
  {ok, [P1]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"In simulating atom_to_list/1", ?_assertEqual(42, P1)}
  ].

bogus_identity_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, atom_to_list_bogus, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, 42, cuter_cerl:empty_tag()).

%% Is an empty atom

atom_nil({_Dir, Fname, Python}) ->
  As = [0, 0],
  Mapping = create_logfile(Fname, As, fun atom_nil_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"It's an empty atom", ?_assertEqual('', P1)}
  , {"It's not an empty atom", ?_assertNotEqual('', P2)}
  ].

atom_nil_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  Y = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, is_atom_nil, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()),
  cuter_log:log_mfa(Fd, {cuter_erlang, is_atom_nil, 1}, [P2], Y, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, Y, false, cuter_cerl:empty_tag()).

%% First letter in an atom

atom_head({_Dir, Fname, Python}) ->
  As = [0],
  Mapping = create_logfile(Fname, As, fun atom_head_logs/2),
  {ok, [P1]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Correct 1st letter of an atom", ?_assertEqual(z, list_to_atom([hd(atom_to_list(P1))]))}
  , {"Make it throw an exception", ?_assertError(badarg, hd(atom_to_list(P1_RV)))}
  ].

atom_head_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, atom_head, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, $z, cuter_cerl:empty_tag()).

%% An atom without its first letter

atom_tail({_Dir, Fname, Python}) ->
  As = [0],
  Mapping = create_logfile(Fname, As, fun atom_tail_logs/2),
  {ok, [P1]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Remove the first letter from an atom", ?_assertEqual(ok, list_to_atom(tl(atom_to_list(P1))))}
  , {"Make it throw an exception", ?_assertError(badarg, tl(atom_to_list(P1_RV)))}
  ].

atom_tail_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, atom_tail, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, ok, cuter_cerl:empty_tag()).

%% List to tuple

lst_to_tpl({_Dir, Fname, Python}) ->
  As = [0],
  Mapping = create_logfile(Fname, As, fun lst_to_tpl_logs/2),
  {ok, [P1]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Convert a list to tuple", ?_assertEqual([ok, 42], P1)}
  , {"Make it throw an exception", ?_assertError(badarg, list_to_tuple(P1_RV))}
  ].

lst_to_tpl_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, list_to_tuple, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, {ok, 42}, cuter_cerl:empty_tag()).

%% Tuple to list

tpl_to_lst({_Dir, Fname, Python}) ->
  As = [0],
  Mapping = create_logfile(Fname, As, fun tpl_to_lst_logs/2),
  {ok, [P1]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  {ok, [P1_RV]} = cuter_solver:solve(Python, Mapping, Fname, 1),
  [ {"Convert a tuple to list", ?_assertEqual({ok, 42}, P1)}
  , {"Make it throw an exception", ?_assertError(badarg, tuple_to_list(P1_RV))}
  ].

tpl_to_lst_logs(Fd, SAs=[P1]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {erlang, tuple_to_list, 1}, [P1], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, [ok, 42], cuter_cerl:empty_tag()).

%% Compare integers (<)

erlang_lt_int({_Dir, Fname, Python}) ->
  As = [0, 0],
  Mapping = create_logfile(Fname, As, fun erlang_lt_int_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Compare integers (<)", ?_assertEqual(true, P1 < P2 andalso is_integer(P1) andalso is_integer(P2))} ].

erlang_lt_int_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, lt_int, 2}, [P1, P2], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).

%% Compare floats (<)

erlang_lt_float({_Dir, Fname, Python}) ->
  As = [0, 0],
  Mapping = create_logfile(Fname, As, fun erlang_lt_float_logs/2),
  {ok, [P1, P2]} = cuter_solver:solve(Python, Mapping, Fname, 42),
  [ {"Compare floats (<)", ?_assertEqual(true, P1 < P2 andalso is_float(P1) andalso is_float(P2))} ].

erlang_lt_float_logs(Fd, SAs=[P1, P2]) ->
  cuter_log:log_symb_params(Fd, SAs),
  X = cuter_symbolic:fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, {cuter_erlang, lt_float, 2}, [P1, P2], X, cuter_cerl:empty_tag()),
  cuter_log:log_equal(Fd, true, X, true, cuter_cerl:empty_tag()).
