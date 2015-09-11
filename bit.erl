-module(bit).
-compile(export_all).

%% Construct bitstrings.

f11(X) ->
  case <<X:5>> =:= <<9:5>> of
    true -> error(not_ok);
    false -> ok
  end.

f12(X, Y, Z) ->
  case <<X:2, Y:3, Z:3>> =:= <<42>> of
    true -> error(not_ok);
    false -> ok
  end.

%% This one takes some time (just to demonstrate the scalability problem
%% with the current implementation).
f13(X, Y, Z) ->
  case <<5:3, X:Y, 2:Z>> =:= <<186>> of
    true -> error(not_ok);
    false -> ok
  end.

%% Match against a constant value.

f21(X) ->
  case X of
    <<42:15>> ->error(not_ok);
    _ -> ok
  end.

f22(X, Y) ->
  case X of
    <<42:Y>> ->error(not_ok);
    _ -> ok
  end.

f23(X, Y) ->
  case <<X:5>> of
    <<9:Y>> ->error(not_ok);
    _ -> ok
  end.

%% Match and binding variables.

f31(X, Y) ->
  case X of
    <<Y:4>> ->error(not_ok);
    _ -> ok
  end.

f32(X, Y, Z) ->
  case X of
    <<2:3, Y:Z, 3>> ->error(not_ok);
    _ -> ok
  end.

f33(X, Y, K, Z) ->
  case Z of
    <<>> -> ok;
    _ ->
      case X of
        <<K:Y, Z/bitstring>> ->error(not_ok);
        _ -> ok
      end
  end.
