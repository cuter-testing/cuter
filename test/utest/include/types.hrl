%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------

%% Types for test generators.
-type f_one() :: fun((any()) -> any()).
-type setup() :: {setup, fun(() -> any()), f_one(), f_one()}.
-type tgen() :: {nonempty_string(), setup()}.
