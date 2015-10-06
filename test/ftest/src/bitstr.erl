-module(bitstr).
-compile(export_all).

%% Construct bitstrings.

-spec f11(integer()) -> ok.
f11(X) ->
  case <<X:7>> =:= <<42:7>> of
    true -> error(not_ok);
    false -> ok
  end.

-spec f12(integer(), integer(), integer()) -> ok.
f12(X, Y, Z) ->
  case <<X:2, Y:3, Z:3>> =:= <<42>> of
    true -> error(not_ok);
    false -> ok
  end.

%% This one takes some time (just to demonstrate the scalability problem
%% with the current implementation).
-spec f13(integer(), integer(), integer()) -> ok.
f13(X, Y, Z) ->
  case <<5:3, X:Y, 2:Z>> =:= <<186>> of
    true -> error(not_ok);
    false -> ok
  end.

%% Match against a constant value.
-spec f21(bitstring()) -> ok.
f21(X) ->
  case X of
    <<42:15>> -> error(not_ok);
    _ -> ok
  end.

-spec f22(bitstring(), integer()) -> ok.
f22(X, Y) ->
  case X of
    <<42:Y>> -> error(not_ok);
    _ -> ok
  end.

-spec f23(integer(), integer()) -> ok.
f23(X, Y) ->
  case <<X:5>> of
    <<9:Y>> -> error(not_ok);
    _ -> ok
  end.

%% Match and binding variables.

-spec f31(bitstring(), integer()) -> ok.
f31(X, Y) ->
  case X of
    <<Y:4>> -> error(not_ok);
    _ -> ok
  end.

-spec f32(bitstring(), integer(), integer()) -> ok.
f32(X, Y, Z) ->
  case X of
    <<2:3, Y:Z, 3>> -> error(not_ok);
    _ -> ok
  end.

-spec f33(bitstring(), integer(), integer(), bitstring()) -> ok.
f33(X, Y, K, Z) ->
  case Z of
    <<>> -> ok;
    _ ->
      case X of
        <<K:Y, Z/bits>> -> error(not_ok);
        _ -> ok
      end
  end.
