-module(examples_for_type_analysis).
-export([f/1, f1/1, f2/1, f3/1, f4/1, f5/1]).

-type t2() :: t1() | atom().
-type t1() :: integer().

-record(rec, {x :: integer(), y :: number()}).

-type tree() :: {integer(), tree(), tree()} | nil.

-type t3(X) :: [X].

%% erl_types:t_fun([erl_types:t_any()], erl_types:t_any())
-spec f(any()) -> any().
f(X) -> X.

%% erl_types:t_fun([erl_types:t_integer()], erl_types:t_atom(ok))
-spec f1(t1()) -> ok.
f1(_X) -> ok.

%% erl_types:t_fun([erl_types:t_sup(erl_types:t_integer(), erl_types:t_atom())], erl_types:t_atom(ok))
-spec f2(t2()) -> ok.
f2(_X) -> ok.

%% erl_types:t_fun([erl_types:t_tuple([erl_types:t_from_term(rec), erl_types:t_integer(), erl_types:t_number()])], erl_types:t_atom(ok)).
-spec f3(#rec{}) -> ok.
f3(_X) -> ok.

%% ??
-spec f4(tree()) -> ok.
f4(_X) -> ok.

%% erl_types:t_fun([erl_types:t_list(erl_types:t_tuple([erl_types:t_from_term(rec), erl_types:t_integer(), erl_types:t_number()]))], erl_types:t_atom(ok)).
-spec f5(t3(#rec{})) -> ok.
f5(_X) -> ok.
