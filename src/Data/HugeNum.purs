module Data.HugeNum
  ( BigNum()
  ) where

import Prelude

import Data.Digit

newtype BigNum = BigNum (Array Digit)
