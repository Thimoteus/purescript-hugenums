module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semiring
import Test.QuickCheck.Laws.Data.Ring
import Type.Proxy (Proxy(..))
import Data.HugeNum

prxHugeNum :: Proxy HugeNum
prxHugeNum = Proxy

main = do
  log "Checking HugeNum instances...\n"
  checkEq prxHugeNum
  checkOrd prxHugeNum
  checkSemiring prxHugeNum
  checkRing prxHugeNum
