-module(no_debug_info).

-export([foo/1]).

-spec foo(integer()) -> integer().
foo(42) -> 0;
foo(I) -> I + 1.
