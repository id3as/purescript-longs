module Data.Long.InternalSpecPE
       ( internalSpecPE
       ) where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Int (Parity(..), Radix, binary, decimal, hexadecimal, octal, radix)
import Data.Long.Internal (class SInfo, Long, Long', SignProxy(..), Signed, ULong, Unsigned)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Ord (abs)
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Test.EUnit (TestSuite, suite, test)
import Global as Number
import Test.Assert (assert)
import Type.Proxy (Proxy(..))

internalSpecPE :: TestSuite
internalSpecPE = do
--  longSpec
  fromStringSpec

-- longSpec :: Effect Unit
-- longSpec = suite "Long" do
--  test "should follow laws" $ liftEffect do
  --  checkEq prxSignedLong
    -- checkOrd prxSignedLong
    -- checkSemiring prxSignedLong
    -- checkRing prxSignedLong
    -- checkCommutativeRing prxSignedLong
    -- -- since degree is only up to int, we can only check for IntInLong
    -- checkEuclideanRing prxIntInSignedLong

    -- checkEq prxUnsignedLong
    -- checkOrd prxUnsignedLong
    -- checkSemiring prxUnsignedLong
    -- checkRing prxUnsignedLong
    -- checkCommutativeRing prxUnsignedLong

--   test "should be built from high and low bits" do
--     quickCheck' \high low ->
--       let l = Internal.fromLowHighBits low high :: Long
--       in Internal.highBits l == high && Internal.lowBits l == low

--   test "should convert ints" $ do
--     quickCheck' \i -> Internal.toInt (Internal.signedLongFromInt i) == Just i
--     quickCheck' \i' ->
--       let i = abs i'
--           l = Internal.unsignedLongFromInt (abs i)
--       in (l >>= Internal.toInt) == Just i

--   -- test "should fail to convert negative ints to unsigned longs" do
--   --   Internal.unsignedLongFromInt (-1) `shouldSatisfy` isNothing

--   test "should convert to strings" $ do
--     quickCheck' \(Radix' r) l ->
--       readSigned r (Internal.toStringAs r l) == Just l

--     quickCheck' \(Radix' r) l ->
--       readUnsigned r (Internal.toStringAs r l) == Just l

--   test "should convert numbers" $ do
--     traverse_ (checkNumber signedProxy)
--       [ 0.0
--       , 9223372036854775807.0
--       , (-9223372036854775808.0)
--       , 2e10
--       ]

--     traverse_ (checkNumber unsignedProxy)
--       [ 0.0
--       , 9223372036854775807.0
--       , (18446744073709551615.0)
--       , 2e10
--       ]

--   test "should reject conversion from non whole numbers" $ do
--     traverse_ (\n -> Internal.fromNumber n :: Maybe Long `shouldSatisfy` isNothing)
--       [ 5.5
--       , 100.1
--       , 200.25
--       , 0.000001
--       -- , 999999999.00000001 -- Number is not precise enough to store the decimal part
--       , Number.nan
--       , Number.infinity
--       ]

--   test "should reject conversion of numbers outside the long range" $ do
--     traverse_ (\n -> Internal.fromNumber n :: Maybe Long `shouldSatisfy` isNothing)
--       [ -10000000000000000000.0 -- Must be big enough to store precision
--       , 10000000000000000000.0
--       ]

--     traverse_ (\n -> Internal.fromNumber n :: Maybe ULong `shouldSatisfy` isNothing)
--       [ -1.0
--       , 20000000000000000000.0
--       ]

--   test "should determine odd/even" do
--     quickCheck' \(l :: Long) -> (Internal.parity l == Even) == (l `mod` two == zero)
--     quickCheck' \(l :: ULong) -> (Internal.parity l == Even) == (l `mod` two == zero)

--   test "should always have positive mods" do
--     quickCheck' \(l1 :: Long) l2 -> Internal.positive $ l1 `mod` l2

--   test "should div, quot, mod, rem by 0 be 0" do
--     traverse_ (\f -> f (Internal.signedLongFromInt 2) zero `shouldEqual` zero)
--       [ div, Internal.quot, mod, Internal.rem ]

-- checkNumber :: forall s. SInfo s => Bounded (Long' s) => SignProxy s -> Number -> Aff Unit
-- checkNumber _ n =
--   (Internal.toNumber <$> (Internal.fromNumber n :: Maybe (Long' s))) `shouldEqual` Just n
fromStringSpec :: TestSuite
fromStringSpec = do
  test "should leave valid strings unchanged" do
    assert $ readSigned decimal "12345" == Just (i2lS 12345)
    assert $ readSigned decimal "+12345" == Just (i2lS 12345)
    assert $ readSigned decimal "-12345" == Just (i2lS (-12345))
    assert $ readSigned decimal "-12345" == Just (i2lS (-12345))

  test "should read signed zeros" do
    assert $ readSigned decimal "0" == Just zero
    assert $ readSigned decimal "-0" == Just zero
    assert $ readSigned decimal "000" == Just zero
    assert $ readSigned decimal "-00" == Just zero

  test "should read unsigned zeros" do
    assert $ readUnsigned decimal "0" == Just zero
    assert $ readUnsigned decimal "-0" == Just zero
    assert $ readUnsigned decimal "000" == Just zero
    assert $ readUnsigned decimal "-00" == Just zero

  test "should return Nothing on empty string" do
    assert $ readSigned decimal "" == Nothing

  test "should disallow negative for unsigned" do
    assert $ readUnsigned decimal "-123" == Nothing

  test "should disallow invalid characters depending on radix" do
    assert $ isJust $ readSigned binary "1010"
    assert $ isJust $ readSigned binary "-1010"
    assert $ isNothing $ readSigned binary "1012"

    assert $ isJust $ readSigned octal "1234"
    assert $ isNothing $ readSigned octal "1834"

    assert $ isJust $ readSigned hexadecimal "1bcd"
    assert $ isJust $ readSigned hexadecimal "1BCd"
    assert $ isNothing $ readSigned hexadecimal "1bcz"

  test "should read at the limits" do
    assert $ readSigned decimal "9223372036854775807" == Just top
    assert $ readSigned decimal "+009223372036854775807" == Just top
    assert $ readSigned decimal "-09223372036854775808" == Just bottom

    assert $ readUnsigned decimal "18446744073709551615" == Just top

    assert $ readSigned hexadecimal "7fffffffffffffff" == Just top
    assert $ readSigned hexadecimal "-8000000000000000" == Just bottom

  test "should fail for overflows" do
    assert $ isNothing $ (spy "rhs" (readSigned decimal "9223372036854775808"))
    assert $ isNothing $ readSigned decimal "-9223372036854775809"

    assert $ isNothing $ readUnsigned decimal "18446744073709551616"

    assert $ isNothing $ readSigned hexadecimal "8000000000000000"
    assert $ isNothing $ readSigned hexadecimal "-8000000000000001"

readSigned :: Radix -> String -> Maybe Long
readSigned = Internal.fromStringAs

readUnsigned :: Radix -> String -> Maybe ULong
readUnsigned = Internal.fromStringAs

i2lS :: Int -> Long
i2lS = Internal.signedLongFromInt

i2lU :: Int -> ULong
i2lU = Internal.unsafeFromInt

prxSignedLong :: Proxy Long
prxSignedLong = Proxy

prxUnsignedLong :: Proxy ULong
prxUnsignedLong = Proxy

-- prxIntInSignedLong :: Proxy IntInSignedLong
-- prxIntInSignedLong = Proxy

signedProxy :: SignProxy Signed
signedProxy = SignProxy

unsignedProxy :: SignProxy Unsigned
unsignedProxy = SignProxy

two :: forall s. (SInfo s) => Long' s
two = Internal.unsafeFromInt 2

-- Helper for Longs within the Int range
-- newtype IntInSignedLong = IntInSignedLong Long
-- instance arbitraryIntInSignedLong :: Arbitrary IntInSignedLong where
--   arbitrary = IntInSignedLong <<< Internal.signedLongFromInt <$> arbitrary

-- derive newtype instance eqIntInSignedLong :: Eq IntInSignedLong
-- derive newtype instance semiringIntInSignedLong :: Semiring IntInSignedLong
-- derive newtype instance ringIntInSignedLong :: Ring IntInSignedLong
-- derive newtype instance commutativeRingIntInSignedLong :: CommutativeRing IntInSignedLong
-- derive newtype instance eucledianRingIntInSignedLong :: EuclideanRing IntInSignedLong

-- newtype Radix' = Radix' Radix
-- instance arbitraryRadix' :: Arbitrary Radix' where
--   arbitrary = chooseInt 2 36 >>= \i ->
--     case radix i of
--       Just r -> pure (Radix' r)
--       Nothing -> arbitrary

-- quickCheck' :: forall a. Testable a => a -> Aff Unit
-- quickCheck' = liftEffect <<< quickCheck
