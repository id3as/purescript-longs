-module(data_long_fFI@foreign).

-export([ zero/0
        , one/0
        , negOne/0
        , uzero/0
        , uone/0
        , maxValue/0
        , maxUnsignedValue/0
        , minValue/0
        , isLong/0
        , fromBits/0
        , fromBytes/0
        , fromBytesLE/0
        , fromBytesBE/0
        , fromInt/0
        , fromNumber/0
        , fromString/0
        , fromValue/0
        , unsigned/0
        , add/1
        , 'and'/1
        , compare/1
        , divide/1
        , equals/1
        , getHighBits/1
        , getHighBitsUnsigned/1
        , getLowBits/1
        , getLowBitsUnsigned/1
        , getNumBitsAbs/1
        , greaterThan/1
        , greaterThanOrEqual/1
        , isEven/1
        , isOdd/1
        , isPositive/1
        , isNegative/1
        , isZero/1
        , lessThan/1
        , lessThanOrEqual/1
        , modulo/1
        , multiply/1
        , negate/1
        , 'not'/1
        , notEquals/1
        , 'or'/1
        , shiftLeft/1
        , shiftRight/1
        , shiftRightUnsigned/1
        , rotateLeft/1
        , rotateRight/1
        , subtract /1
        , toBytes/2
        , toInt/1
        , toNumber/1
        , toSigned/1
        , toString/1
        , toUnsigned/1
        , 'xor'/1
        ]).


%% Constants
zero() -> 0.
one() -> 1.
negOne() -> -1.
uzero() -> 0.
uone() -> 1.
maxValue() -> 16#7fffffffffffffff.
maxUnsignedValue() -> 16#ffffffffffffffff.
minValue() -> -16#8000000000000000.

isLong() ->
  fun(Num) when is_integer(Num),
                Num =< 16#ffffffffffffffff ->
      true;
     (_) ->
      false
  end.

fromBits() ->
  fun(Low, High, true) ->
      <<X:64/little-unsigned-integer>> = <<Low:32/little-integer, High:32/little-integer>>,
      X;
     (Low, High, false) ->
      <<X:64/little-signed-integer>> = <<Low:32/little-integer, High:32/little-integer>>,
      X
  end.

fromBytes() ->
  fun(Array, IsUnsigned, IsLittleEndian) ->
      Bin = list_to_binary(array:to_list(Array)),
      case {IsLittleEndian, IsUnsigned} of
        {true, true} ->
          <<X:64/little-unsigned-integer>> = Bin;
        {true, false} ->
          <<X:64/little-signed-integer>> = Bin;
        {false, true} ->
          <<X:64/big-unsigned-integer>> = Bin;
        {false, false} ->
          <<X:64/big-signed-integer>> = Bin
      end,
      X
  end.

%% Creates a Long from its little endian byte representation.
fromBytesLE() ->
  fun(Array, IsUnsigned) ->
      (fromBytes())(Array, IsUnsigned, true)
  end.

%% Creates a Long from its big endian byte representation.
fromBytesBE() ->
  fun(Array, IsUnsigned) ->
      (fromBytes())(Array, IsUnsigned, false)
  end.

fromInt() -> fun(A, _IsUnsigned) -> A end.

fromNumber() -> fun(A, _IsUnsigned) -> trunc(A) end.

fromString() ->
  fun(String, _IsUnsigned, Radix) ->
      binary_to_integer(String, Radix)
  end.

fromValue() ->
  fun(A) when is_integer(A),
              A < 16#ffffffffffffffff ->
      A
  end.

unsigned() ->
  fun(A) when A < 0 -> false;
     (_A) -> true
  end.

%% Methods
add(A) -> fun(B) -> A + B end.
'and'(A) -> fun(B) -> A band B end.

compare(A) -> fun(B) when A > B -> 1;
                 (B) when A == B -> 0;
                 (_) -> -1 end.

divide(A) -> fun(B) -> A div B end.
equals(A) -> fun(B) -> A == B end.
getHighBits(A) -> A bsr 32.
getHighBitsUnsigned(A) -> A bsr 32.
getLowBits(A) -> A band 16#ffffffff.
getLowBitsUnsigned(A) -> A band 16#ffffffff.

getNumBitsAbs(A) -> getNumBitsAbs(A, 0).
getNumBitsAbs(A, N) when A == 0 -> N;
getNumBitsAbs(A, N) -> getNumBitsAbs(A bsr 1, N + 1).

greaterThan(A) -> fun(B) -> A > B end.
greaterThanOrEqual(A) -> fun(B) -> A >= B end.
isEven(A) when A >= 0 -> (A band 1) == 0.
isOdd(A) when A > 0 -> not isEven(A).
isPositive(A) -> A > 0.
isNegative(A) -> A < 0.
isZero(A) -> A == 0.
lessThan(A) -> fun(B) -> A < B end.
lessThanOrEqual(A) -> fun(B) -> A =< B end.
modulo(A) -> fun(B) -> A rem B end.
multiply(A) -> fun(B) -> A * B end.
negate(A) -> -A.
'not'(A) -> bnot A.
notEquals(A) -> fun(B) -> A /= B end.
'or'(A) -> fun(B) -> A bor B end.
shiftLeft(A) -> fun(B) -> A bsl B end.
shiftRight(A) -> fun(B) -> A bsr B end.
shiftRightUnsigned(A) -> fun(B) -> A bsr B end.
rotateLeft(A) -> fun(Num) -> ((A bsl Num) band 16#ffffffffffffffff) bor (A bsr (64 - Num)) end.
rotateRight(A) -> fun(Num) -> (A bsr Num) bor ((A bsl (64 - Num)) band 16#ffffffffffffffff) end.
subtract(A) -> fun(B) -> A - B end.

toBytes(A, true) -> array:from_list (binary_to_list(<<A:64/little-integer>>));
toBytes(A, false) -> array:from_list (binary_to_list(<<A:64/big-integer>>)).

toInt(A) -> A band 16#ffffffff.
toNumber(A) -> A * 1.0.
toSigned(A) when A =< 16#7fffffffffffffff -> A;
toSigned(A) -> -1 * ((bnot (A band 16#fffffffffffffffe)) band 16#ffffffffffffffff).
toString(A) -> fun(R) -> integer_to_binary (A, R) end.
toUnsigned(A) when A >= 0 -> A;
toUnsigned(A) -> <<X:64/unsigned-integer>> = <<A:64/signed-integer>>, X.
'xor'(A) -> fun(B) -> A bxor B end.
