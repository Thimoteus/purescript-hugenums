module Data.Digit
  ( Digit()
  ) where

import Prelude
import Data.Char (toCharCode, fromCharCode)
import Data.Maybe (Maybe(..))

newtype Digit = Digit Char

mkDigit :: Char -> Maybe Digit
mkDigit c
  | toCharCode c >= 48 && toCharCode c <= 57 = Just (Digit c)
  | otherwise = Nothing
