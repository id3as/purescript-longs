module Test.Main where

import Prelude

import Data.Long.FFISpecPE (ffiSpecPE)
import Data.Long.InternalSpecPE (internalSpecPE)
import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Console (log)
import Erl.Test.EUnit (runTests, suite, test)
import Test.Assert (assert, assertEqual)


main :: Effect Unit
main =
  void $ runTests do
    suite "Purerl tests" do
      suite "FFI purerl tests" do
        ffiSpecPE
      suite "Internal purerl tests" do
        internalSpecPE
