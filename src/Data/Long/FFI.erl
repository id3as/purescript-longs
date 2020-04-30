-module(data_long_FFI@foreign).

-export([ add/2
        , 'and'/2
        , compare/2
        , divide/2
        , equals/2
        , getHighBits/1
        , getHighBitsUnsigned/1
        , getLowBits/1
        , getLowBitsUnsigned/1
        , getNumBitsAbs/1
        , greaterThan/2
        , greaterThanOrEqual/2
        , isEven/1
        , isOdd/1
        , isPositive/1
        , isZero/1
        , lessThan/2
        , lessThanOrEqual/2
        , modulo/2
        , multiply/2
        , negate/1
        , 'not'/1
        , notEquals/2
        , 'or'/2
        , shiftLeft/2
        , shiftRight/2
        , shiftRightUnsigned/2
        , rotateLeft/2
        , rotateRight/2
        , subtract /2
        , toBytes/2
        , toInt/1
        , toNumber/1
        , toSigned/1
        , toString/2
        , toUnsigned/1
        , 'xor'/2
        ]
  ).

add(A, B) -> A + B.

'and'(A, B) -> A band B.

compare(A, B) when A > B -> 1;
compare(A, B) when A == B -> 0;
compare(_, _) -> -1.

divide(A, B) -> A div B.
equals(A, B) -> A == B.

getHighBits(A) -> A bsr 32.

getHighBitsUnsigned(A) -> A bsr 32.

getLowBits(A) -> A band 16#ffffffff.

getLowBitsUnsigned(A) -> A band 16#ffffffff.


getNumBitsAbs(A) -> getNumBitsAbs(A, 0).

getNumBitsAbs(A, N) when A == 0 -> N;
getNumBitsAbs(A, N) -> getNumBitsAbs(A bsr 1, N + 1).

greaterThan(A, B) -> A > B.
greaterThanOrEqual(A, B) -> A >= B.
isEven(A) when A >= 0 -> (A band 1) == 0.
isOdd(A) when A > 0 -> not isEven(A).
isPositive(A) -> A > 0.
isZero(A) -> A == 0.
lessThan(A, B) -> A < B.
lessThanOrEqual(A, B) -> A =< B.
modulo(A, B) -> A rem B.
multiply(A, B) -> A * B.
negate(A) -> -A.
'not'(A) -> bnot A.
notEquals(A, B) -> A /= B.
'or'(A, B) -> A bor B.
shiftLeft(A, B) -> A bsl B.
shiftRight(A, B) -> A bsr B.

shiftRightUnsigned(A, B) -> A bsr B.

rotateLeft(A, Num) ->
  ((A bsl Num) band 16#ffffffffffffffff) bor (A bsr (64 - Num)).

rotateRight(A, Num) ->
  (A bsr Num) bor ((A bsl (64 - Num)) band 16#ffffffffffffffff).

subtract(A, B) -> A - B.

toBytes(A, true) -> array:from_list (binary_to_list(<<A:64/little-integer>>));
toBytes(A, false) -> array:from_list (binary_to_list(<<A:64/big-integer>>)).

toInt(A) -> A band 16#ffffffff.

toNumber(A) -> A * 1.0.

toSigned(A) -> A.

toString(A, R) -> integer_to_binary (A, R).

toUnsigned(A) -> A.

'xor'(A, B) -> A bxor B.
