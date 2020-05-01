-module(data_long_FFI@foreign).

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
        , unsigned/1
        , add/0
        , 'and'/0
        , compare/0
        , divide/0
        , equals/0
        , getHighBits/1
        , getHighBitsUnsigned/1
        , getLowBits/1
        , getLowBitsUnsigned/1
        , getNumBitsAbs/1
        , greaterThan/0
        , greaterThanOrEqual/0
        , isEven/1
        , isOdd/1
        , isPositive/1
        , isNegative/1
        , isZero/1
        , lessThan/0
        , lessThanOrEqual/0
        , modulo/0
        , multiply/0
        , negate/1
        , 'not'/1
        , notEquals/0
        , 'or'/0
        , shiftLeft/0
        , shiftRight/0
        , shiftRightUnsigned/0
        , rotateLeft/0
        , rotateRight/0
        , subtract /0
        , toBytes/0
        , toInt/1
        , toNumber/1
        , toSigned/1
        , toString/0
        , toUnsigned/1
        , 'xor'/0
        ]).


%% Constants
zero() -> 0.
one() -> 1.
negOne() -> -1.
uzero() -> 0.
uone() -> 1.
maxValue() -> fromBits(16#ffffffffffffffff, 16#ffffffff).
maxUnsignedValue() -> fromBits(16#ffffffffffffffff, 16#ffffffff).
minValue() -> fromBits(0, 16#ffffffff).

%% Utilities
%% Object.defineProperty(Long.prototype, "__isLong__", { value: true });
%% function isLong(obj) {
%%     return (obj && obj["__isLong__"]) === true;
%% }
isLong(A) -> A.

fromBits(Low, High, _A) -> (High bsl 32) band Low.


%% Long.fromBytes = function fromBytes(bytes, unsigned, le) {
%%     return le ? Long.fromBytesLE(bytes, unsigned) : Long.fromBytesBE(bytes, unsigned);
%% };
fromBytes(A, _B) -> A.

%% Long.fromBytesLE = function fromBytesLE(bytes, unsigned) {
%%     return new Long(
%%         bytes[0]       |
%%         bytes[1] <<  8 |
%%         bytes[2] << 16 |
%%         bytes[3] << 24,
%%         bytes[4]       |
%%         bytes[5] <<  8 |
%%         bytes[6] << 16 |
%%         bytes[7] << 24,
%%         unsigned
%%     );
%% };
%% Creates a Long from its little endian byte representation.
fromBytesLE(A, _B) -> A.

%% Long.fromBytesBE = function fromBytesBE(bytes, unsigned) {
%%     return new Long(
%%         bytes[4] << 24 |
%%         bytes[5] << 16 |
%%         bytes[6] <<  8 |
%%         bytes[7],
%%         bytes[0] << 24 |
%%         bytes[1] << 16 |
%%         bytes[2] <<  8 |
%%         bytes[3],
%%         unsigned
%%     );
%% };
%% Creates a Long from its big endian byte representation.
fromBytesBE(A, _B, _C) -> A.

fromInt(A, _B) -> A.

fromNumber(A, _B) -> A.

fromString(A, _B, _C) -> A.

fromValue(A, _B) -> A.

unsigned(A) -> A.

%% Methods
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
isNegative(A) -> A < 0.
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
