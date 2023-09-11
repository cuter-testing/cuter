-module(map).
-export([ simple/1, simple_squared/1
        , simple_extend/1, extend_empty/1
        , simple_match/1, match_cases/1, match_many/2
        , match_lit/1, match_var/1, match_embed/1
        , maps_get/1, maps_put/1
        ]).

-spec simple(integer()) -> #{value => float()}.
simple(X) ->
    #{ value => 10 / (X + 4) }.

-spec simple_squared(integer()) -> #{value => float()}.
simple_squared(X) ->
    #{ value => 10 / (X*X - 4) }.

-spec simple_extend(integer()) -> #{value => float()}.
simple_extend(X) ->
    A = #{ a => X },
    A#{ value => 10 / (X*X - 4) }.

-spec extend_empty(integer()) -> #{value => float()}.
extend_empty(X) ->
    A = #{},
    A#{ value => 10 / (X*X - 4) }.

-spec simple_match(integer()) -> float().
simple_match(X) ->
    #{ var := A } = #{ var => X + 2 },
    10 / A.

-spec match_cases(integer()) -> float().
match_cases(X) ->
    M = case X > 50 of
        true -> #{var1 => X};
        false -> #{var2 => X}
    end,
    case M of
        #{ var1 := A } -> 1 / (60 - A);
        #{ var2 := A } -> 2 / (40 - A)
    end.

-spec match_many(integer(), integer()) -> float().
match_many(X, Y) ->
    case #{ a => X+2, b => Y+4, key => value } of
        #{ a := A, b := B } -> 10 / (A + B);
        _ -> 0
    end.

-spec match_lit(integer()) -> float().
match_lit(X) ->
    case #{ val => X} of
        #{ val := 100 } -> error(error);
        _ -> ok
    end.

-spec match_var(integer()) -> float().
match_var(X) ->
    Y = 200,
    case #{ val => X + 1 } of
        #{ val := Y } -> error(error);
        _ -> ok
    end.

-spec match_embed(integer()) -> float().
match_embed(X) ->
    Y = b,
    M = #{ a => #{ b => X + 20 } },
    case M of
        #{ a := #{ Y := A } } -> 10 / A;
        _ -> 0
    end.

-spec maps_get(integer()) -> float().
maps_get(X) ->
    M = #{var => X + 2},
    10 / maps:get(var, M).

-spec maps_put(integer()) -> float().
maps_put(X) ->
    M = #{},
    Z = maps:put(value, X, M),
    Y = maps:get(value, Z),
    10 / (Y+1).

