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
        , unsigned/1
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
maxValue() -> (fromBits())(16#ffffffffffffffff, 16#ffffffff, true).
maxUnsignedValue() -> (fromBits())(16#ffffffffffffffff, 16#ffffffff, false).
minValue() -> (fromBits())(0, 16#ffffffff, true).

%% Utilities
%% Object.defineProperty(Long.prototype, "__isLong__", { value: true });
%% function isLong(obj) {
%%     return (obj && obj["__isLong__"]) === true;
%% }
isLong() -> fun(A) -> A end.

fromBits() -> fun(Low, High, _A) -> (High bsl 32) band Low end.


%% Long.fromBytes = function fromBytes(bytes, unsigned, le) {
%%     return le ? Long.fromBytesLE(bytes, unsigned) : Long.fromBytesBE(bytes, unsigned);
%% };
fromBytes() -> fun(A, _B, _C) -> A end.

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
fromBytesLE() -> fun(A, _B, _C) -> A end.

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
fromBytesBE() -> fun(A, _B, _C) -> A end.

fromInt() -> fun(A, _B) -> A end.

fromNumber() -> fun(A, _B) -> A end.

fromString() -> fun(A, _B, _C) -> A end.

fromValue() -> fun(A, _B) -> A end.

unsigned(A) -> A.

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
toSigned(A) -> A.
toString(A) -> fun(R) -> integer_to_binary (A, R) end.
toUnsigned(A) -> A.
'xor'(A) -> fun(B) -> A bxor B end.
