module Data.Long.FFISpecPE
       ( ffiSpecPE
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Int (decimal)
import Data.Long.FFI (Long)
import Data.Long.FFI as FFI
import Data.Long.Internal as Internal
import Data.Long.TestUtilsPE (i2lS, i2lU, isBigEndianV, isLittleEndianV, isSignedV, isUnsignedV, unsafeS2lS, unsafeS2lU)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Effect.Exception (Error, error)
import Erl.Test.EUnit (TestSuite, suite, test)
import Foreign (unsafeToForeign)
import Test.Assert (assert)

ffiSpecPE :: TestSuite
ffiSpecPE = do
  test "should have correct values for constants" do
    assert $ FFI.zero == (i2lS 0)
    assert $ FFI.one == (i2lS 1)
    assert $ FFI.negOne == (i2lS (-1))
    assert $ FFI.uzero == (i2lU 0)
    assert $ FFI.uone == (i2lU 1)
    assert $ FFI.maxValue == (unsafeS2lS "9223372036854775807")
    assert $ FFI.minValue == (unsafeS2lS "-9223372036854775808")
    assert $ FFI.maxUnsignedValue == (unsafeS2lU "18446744073709551615")

  test "should create longs" do
    liftEffect $ assert $ FFI.isLong (unsafeToForeign FFI.zero)
    assert $ (runFn3 FFI.fromBits sampleS.low sampleS.high isSignedV) == sampleS.value
    assert $ (runFn3 FFI.fromBits sampleU.low sampleU.high isUnsignedV) == sampleU.value
    assert $ (runFn3 FFI.fromBytes sampleS.beBytes isSignedV isBigEndianV) == sampleS.value
    assert $ (runFn2 FFI.fromBytesLE sampleS.leBytes isSignedV) == sampleS.value
    assert $ (runFn2 FFI.fromBytesBE sampleS.beBytes isSignedV) == sampleS.value
    assert $ (runFn2 FFI.fromInt 2 isSignedV) == (unsafeS2lS "2")
    assert $ (runFn2 FFI.fromNumber 2.0 isSignedV) == (unsafeS2lS "2")
    assert $ (runFn2 FFI.fromNumber 2.0 isSignedV) == (unsafeS2lS "2")
    assert =<< ( ((==) (runFn2 FFI.fromInt 2 isSignedV)) <$> runEffectFn3 FFI.fromString "2" isSignedV decimal)
    expectError $ runEffectFn3 FFI.fromString "2-2" isSignedV decimal

  test "should access fields" do
    assert $ FFI.unsigned (i2lU 2) == isUnsignedV

  test "should access methods" do
    assert $ (FFI.add (i2lS 2) (i2lS 3)) == (i2lS 5)
    assert $ (FFI.and (i2lS 2) (i2lS 1)) == (i2lS 0)

    assert $ (FFI.compare (i2lS 1) (i2lS 2)) == (-1)
    assert $ (FFI.compare (i2lS 2) (i2lS 2)) == 0
    assert $ (FFI.compare (i2lS 2) (i2lS 1)) == 1

    assert $ (FFI.divide (i2lS 8) (i2lS 3)) == (i2lS 2)
    assert $ (Internal.numberBitsToInt $ FFI.getHighBits sampleS.value) == sampleS.high
    assert $ (Internal.numberBitsToInt $ (FFI.getHighBitsUnsigned sampleU.value)) == sampleU.high
    assert $ (Internal.numberBitsToInt $ FFI.getLowBits sampleS.value) == sampleS.low
    assert $ (Internal.numberBitsToInt $ FFI.getLowBitsUnsigned sampleU.value) == sampleU.low
    assert $ (_ `FFI.greaterThan` (i2lS 2)) (i2lS 5)
    assert $ (_ `FFI.greaterThanOrEqual` (i2lS 5)) (i2lS 5)
    assert $ FFI.isEven (i2lS 6)
    assert $ FFI.isNegative (i2lS (-6))
    assert $ FFI.isOdd (i2lS 5)
    assert $ FFI.isPositive (i2lS 5)
    assert $ FFI.isZero FFI.zero
    assert $ (_ `FFI.lessThan` (i2lS 5)) (i2lS 2)
    assert $ (_ `FFI.lessThanOrEqual` (i2lS 5)) (i2lS 5)

    -- -- modulo, note the sign of the answers
    assert $ (FFI.modulo (i2lS 5) (i2lS 3)) == (i2lS 2)
    assert $ (FFI.modulo (i2lS (-5)) (i2lS 3)) == (i2lS (-2))
    assert $ (FFI.modulo (i2lS 5) (i2lS (-3))) == (i2lS 2)
    assert $ (FFI.modulo (i2lS (-5)) (i2lS (-3))) == (i2lS (-2))

    assert $ (FFI.multiply (i2lS 5) (i2lS 3)) == (i2lS 15)
    assert $ (FFI.negate (i2lS 5)) == (i2lS (-5))
    assert $ (FFI.not (i2lS (-12345))) == (i2lS 12344)
    assert $ (FFI.notEquals (i2lS (-12345)) (i2lS 12344) )
    assert $ (FFI.or (i2lS 11) (i2lS 5)) == (i2lS 15)
    assert $ (FFI.shiftLeft (i2lS 11) (i2lS 2)) == (i2lS 44)
    assert $ (FFI.shiftRight (i2lS (-11)) (i2lS 2)) == (i2lS (-3))
    assert $ (FFI.shiftRightUnsigned (unsafeS2lU "18446744073709551605") (i2lU 2)) == (unsafeS2lU "4611686018427387901")
    -- -- TODO rotateLeft, i2lS 2 (FFI.subtract (i2lS 2) (i2lS 3)) == (i2lS -1)
    assert $ (FFI.toBytes sampleS.value isLittleEndianV) == sampleS.leBytes
    assert $ (FFI.toBytes sampleS.value isBigEndianV) == sampleS.beBytes
    assert $ (FFI.toInt (i2lS 2)) == 2
    -- -- out of range gets clipped
    assert $ (FFI.toInt (unsafeS2lS "100000000000")) == (Internal.numberBitsToInt 100000000000.0)

    -- -- can lose precision when converting to number
    assert $ (FFI.toNumber (unsafeS2lS "9007199254740993")) == 9007199254740992.0

    assert $ (FFI.toSigned (unsafeS2lU "18446744073709551605")) == (i2lS (-11))
    assert $ (FFI.toUnsigned (i2lS (-11))) == (unsafeS2lU "18446744073709551605")
    assert $ (FFI.xor (i2lS 11) (i2lS 5)) == (i2lS 14)

-- Sample

sampleS ::
  { value :: Long
  , high :: Int
  , low :: Int
  , beBytes :: Array Int
  , leBytes :: Array Int
  }
sampleS =
  { value: unsafeS2lS "-107374182489"
  , high: -26
  , low: -89
  , beBytes
  , leBytes: Array.reverse beBytes
  }
  where
    beBytes = [255, 255, 255, 230, 255, 255, 255, 167]

sampleU ::
  { value :: Long
  , high :: Int
  , low :: Int
  }
sampleU =
  { value: unsafeS2lU "18446743983515238366"
  , high: -22
  , low: -34
  }

expectError
  :: forall m t
   . MonadError Error m
  => m t
  -> m Unit
expectError a = do
  e <- try a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"
