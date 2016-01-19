module Test.Main where

import Prelude
import Test.Unit (test, runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Data.HugeNum

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main = runTest do
  test "the commutative property" do
    quickCheck theCommutativeProperty
