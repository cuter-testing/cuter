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
  case <<5:3, X:Y, 2:Z>> of
    <<186>> -> error(not_ok);
    _ -> ok
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

-spec f24(<<_:6>>, integer()) -> ok.
f24(X, Y) ->
  case X of
    <<42:Y>> -> error(not_ok);
    _ -> ok
  end.

-spec f25(<<_:_*3>>, integer()) -> ok.
f25(X, Y) ->
  case X of
    <<42:Y>> -> error(not_ok);
    _ -> ok
  end.

-spec f26(<<_:6, _:_*2>>, integer()) -> ok.
f26(X, Y) ->
  case X of
    <<42:Y>> -> error(not_ok);
    _ -> ok
  end.

-spec f27(<<_:4>>, integer()) -> ok.
f27(X, Y) ->
  case X of
    <<42:Y>> -> error(not_ok);
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

-spec f34(<<_:_*3>>, integer()) -> ok.
f34(X, Y) ->
  case X of
    <<Y:4>> -> error(not_ok);
    _ -> ok
  end.

-spec f35(<<_:6, _:_*7>>) -> ok.
f35(X) ->
  case X of
    <<_:42>> -> error(not_ok);
    _ -> ok
  end.

-spec f36(<<_:6, _:_*7>> | <<_:_*4>>) -> ok.
f36(X) ->
  case X of
    <<_:42>> -> error(not_ok);
    _ -> ok
  end.

-spec f37(<<_:6, _:_*7>> | <<_:_*4>> | <<_:_*6>>) -> ok.
f37(X) ->
  case X of
    <<_:42>> -> error(not_ok);
    _ -> ok
  end.

-spec byte_sz(bitstring()) -> ok.
byte_sz(Bits) ->
  case byte_size(Bits) of
    Sz when Sz < 2 -> ok
  end.

-spec bit_sz(bitstring()) -> ok.
bit_sz(Bits) ->
  case bit_size(Bits) of
    Sz when Sz < 4 -> ok
  end.

-spec fbsl(integer(), integer()) -> ok.
fbsl(X, Y) ->
  case X bsl Y of
    42 -> error(bug);
    _ -> ok
  end.

-spec bsl_big(integer()) -> ok.
bsl_big(X) ->
  M = 123456789012345,
  L = 123456789012345678901234567890,
  XL = 1234567890123456789012345678901234567890123456789012345678901234567890,
  case X bsl 4 of
    Y when Y > M, Y < L -> error(bug1);
    Y when Y > L, Y < XL -> error(bug2);
    Y when Y > XL -> error(bug3);
    _ -> ok
  end.

-spec fbsr(integer(), integer()) -> ok.
fbsr(X, Y) ->
  case X bsr Y of
    42 -> error(bug);
    _ -> ok
  end.

-spec bsr_big(integer()) -> ok.
bsr_big(X) ->
  M = 123456789012345,
  L = 123456789012345678901234567890,
  XL = 1234567890123456789012345678901234567890123456789012345678901234567890,
  case X bsr 4 of
    Y when Y > M, Y < L -> error(bug1);
    Y when Y > L, Y < XL -> error(bug2);
    Y when Y > XL -> error(bug3);
    _ -> ok
  end.

-spec fbnot(integer()) -> ok.
fbnot(X) ->
  case bnot X of
    42 -> error(bug);
    _ -> ok
  end.

-spec bnot_big(integer()) -> ok.
bnot_big(X) ->
  M = 123456789012345,
  L = 123456789012345678901234567890,
  XL = 1234567890123456789012345678901234567890123456789012345678901234567890,
  case bnot X of
    Y when Y > M, Y < L -> error(bug1);
    Y when Y > L, Y < XL -> error(bug2);
    Y when Y > XL -> error(bug3);
    _ -> ok
  end.

-spec fband(integer(), integer()) -> ok.
fband(X, Y) ->
  case X band Y of
    42 -> error(bug);
    _ -> ok
  end.

-spec fband2(integer(), integer()) -> ok.
fband2(X, Y) ->
  case X band Y of
    1267650600228229401496703205376 -> error(bug); % 2^100
    _ -> ok
  end.

-spec fband_neg(integer(), integer()) -> ok.
fband_neg(X, Y) ->
  case X band Y of
    -42 -> error(bug);
    _ -> ok
  end.

-spec fband3(integer(), integer()) -> ok.
fband3(X, Y) ->
  case (X + Y) band (X - Y) of
    1208425819634629144706176 -> error(bug);
    _ -> ok
  end.

-spec fband2_neg(integer(), integer()) -> ok.
fband2_neg(X, Y) ->
  case X band Y of
    -1299341865233935136534120785510400 -> error(bug); % - 2^110 - 2^100
    _ -> ok
  end.
