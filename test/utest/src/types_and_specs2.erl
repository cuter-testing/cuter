-module(types_and_specs2).

-export([foo/0]).

-export_type([e1/1]).

-type e2(Y) :: Y.
-type e1(X) :: e2(X).

-spec foo() -> ok.
foo() -> ok.
