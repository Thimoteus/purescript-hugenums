module Data.HugeNum
  ( HugeNum()
  , fromNumber
  , toNumber
  , lessPrecise
  , integerPart
  , fractionalPart
  , parseScientific , Sign(..), parseNumber, NumberStyle(..)
  ) where

import Prelude
import Global (readFloat)

import Data.String (toCharArray, fromCharArray, contains)
import Data.Array (span, drop, take, mapMaybe, zipWith, length, filter, uncons,
                  insertAt, replicate, dropWhile, reverse)
import Data.Foldable (foldl, all)
import Data.Maybe.Unsafe (fromJust)
import Data.Int (round)
import Data.Digit

data Sign = Plus | Minus

instance eqSign :: Eq Sign where
  eq Plus Plus = true
  eq Minus Minus = true
  eq _ _ = false

instance ordSign :: Ord Sign where
  compare Plus Minus = GT
  compare Minus Plus = LT
  compare _ _ = EQ

type HugeRec = { digits :: Array Digit, decimal :: Int, sign :: Sign }
newtype HugeNum = HugeNum HugeRec

instance showHugeNum :: Show HugeNum where
  show = toString

instance eqHugeNum :: Eq HugeNum where
  eq x y
    | isZero x && isZero y = true
    | otherwise = strictlyEqual x y

strictlyEqual :: HugeNum -> HugeNum -> Boolean
strictlyEqual (HugeNum r1) (HugeNum r2) =
  r1.decimal == r2.decimal && r1.digits == r2.digits && r1.sign == r2.sign

instance ordHugeNum :: Ord HugeNum where
  compare = compareHugeNum

rec :: HugeNum -> HugeRec
rec (HugeNum r) = r

compareHugeNum :: HugeNum -> HugeNum -> Ordering
compareHugeNum x@(HugeNum r1) y@(HugeNum r2)
  | x == y = EQ
  | r1.decimal > r2.decimal = GT
  | r1.decimal < r2.decimal = LT
  | r1.sign < r2.sign = LT
  | r1.sign > r2.sign = GT
  | otherwise = z where
    dec = r1.decimal
    r = equivalize x y
    s = rec r.fst
    m = rec r.snd
    x' = if x == r.fst then s else m
    y' = if y == r.fst then s else m
    z = case compare (take dec r1.digits) (take dec r2.digits) of
             EQ -> compare (drop dec x'.digits) (drop dec y'.digits)
             x -> x

dropZeroes :: HugeNum -> HugeNum
dropZeroes (HugeNum r) = HugeNum z where
  iPutMyThingDown = reverse (drop r.decimal r.digits)
  flipIt = dropWhile (== _zero) iPutMyThingDown
  andReverseIt = reverse flipIt
  z = r { digits = take r.decimal r.digits ++ andReverseIt }

equivalize :: HugeNum -> HugeNum -> { fst :: HugeNum, snd :: HugeNum }
equivalize x@(HugeNum r1) y@(HugeNum r2)
  | length (drop r1.decimal r1.digits) == length (drop r2.decimal r2.digits) = { fst: x, snd: y }
  | otherwise = z where
    test = x `lessPrecise` y
    lesser = if test then r1 else r2
    greater = if test then r2 else r1
    lesserDecimal = length (drop greater.decimal greater.digits) - length (drop lesser.decimal lesser.digits)
    zeroes = replicate lesserDecimal _zero
    lesser' = lesser { digits = lesser.digits ++ zeroes }
    z = { fst: HugeNum lesser', snd: HugeNum greater }

lessPrecise :: HugeNum -> HugeNum -> Boolean
lessPrecise (HugeNum r1) (HugeNum r2) =
  length (drop r1.decimal r1.digits) < length (drop r2.decimal r2.digits)

integerPart :: HugeNum -> HugeNum
integerPart (HugeNum r) =
  HugeNum (r { digits = take r.decimal r.digits ++ [_zero]})

fractionalPart :: HugeNum -> HugeNum
fractionalPart (HugeNum r) =
  HugeNum (r { digits = [_zero] ++ drop r.decimal r.digits })

-- | May lose precision if the argument is too large.
toNumber :: HugeNum -> Number
toNumber = readFloat <<< toString

toString :: HugeNum -> String
toString (HugeNum r) =
  let charray = map toChar r.digits
      numray = fromJust (insertAt r.decimal '.' charray)
      sign = case r.sign of
                  Plus -> []
                  Minus -> ['-']
   in "HugeNum " ++ fromCharArray (sign ++ numray)

zeroHugeNum :: HugeNum
zeroHugeNum = HugeNum { digits: [_zero, _zero], decimal: 1 , sign: Plus }

oneHugeNum :: HugeNum
oneHugeNum = HugeNum { digits: [_one, _one], decimal: 1, sign: Plus }

data NumberStyle = Float | Integral | Scientific
instance showNumberStyle :: Show NumberStyle where
  show Float = "Float"
  show Integral = "Integral"
  show Scientific = "Scientific"

-- | May lose precision if the argument is too large.
-- | For example, the fractional part of `9000000000000000.5` is unrecoverable.
parseNumber :: Number -> NumberStyle
parseNumber n
  | contains "e" (show n) = Scientific
  | contains "." (show n) = Float
  | otherwise = Integral

floatToHugeNum :: Number -> HugeNum
floatToHugeNum n = HugeNum r where
  pos = n > zero
  split = if pos
             then span (/= '.') (toCharArray $ show n)
             else span (/= '.') (drop 1 $ toCharArray $ show n)
  big = split.init
  small = drop 1 split.rest
  sign = if n < zero then Minus else Plus
  r = { digits: mapMaybe fromChar $ big ++ small
      , decimal: length big
      , sign: sign }

integralToHugeNum :: Number -> HugeNum
integralToHugeNum n =
  let integral = toCharArray $ show n
      fractional = pure _zero
      sign = if n < zero then Minus else Plus
      decimal = case sign of
                     Minus -> length integral - 1
                     Plus -> length integral
   in HugeNum { digits: mapMaybe fromChar integral ++ fractional
              , decimal: decimal
              , sign: sign }

scientificToHugeNum :: Number -> HugeNum
scientificToHugeNum n = HugeNum r where
  parsed = parseScientific n
  r = case parsed.expSign of
           Plus -> case parsed.sign of
                        Plus -> parsePlusPlus parsed.exponent parsed.base
                        Minus -> parsePlusMinus parsed.exponent parsed.base
           Minus -> case parsed.sign of
                         Plus -> parseMinusPlus parsed.exponent parsed.base
                         Minus -> parseMinusMinus parsed.exponent parsed.base

parseScientific :: Number -> _
parseScientific n = z where
  split = span (/= 'e') (toCharArray $ show n)
  base = filter (/= '.') split.init
  sign = if n < zero then Minus else Plus
  signSplit = fromJust $ uncons $ drop 1 split.rest
  expSign = case signSplit.head of
                 '+' -> Plus
                 _ -> Minus
  exponent = round $ readFloat $ fromCharArray signSplit.tail
  z = { exponent: exponent, expSign: expSign, base: base, sign: sign }

parsePlusPlus :: Int -> Array Char -> HugeRec
parsePlusPlus exp base = r where
  zeroCardinality = exp - length base + 2
  zeroes = replicate zeroCardinality _zero
  digits = mapMaybe fromChar base ++ zeroes
  decimal = 1 + exp
  r = { digits: digits, decimal: decimal, sign: Plus }

parsePlusMinus :: Int -> Array Char -> HugeRec
parsePlusMinus exp base =
  let r = parsePlusPlus exp base
   in r { sign = Minus, digits = r.digits ++ [_zero] }

parseMinusPlus :: Int -> Array Char -> HugeRec
parseMinusPlus exp base = r where
  zeroes = replicate exp _zero
  digits = zeroes ++ mapMaybe fromChar base
  decimal = 1
  r = { digits: digits, decimal: decimal, sign: Plus }

parseMinusMinus :: Int -> Array Char -> HugeRec
parseMinusMinus exp base =
  let r = parseMinusPlus exp base
   in r { sign = Minus }

fromNumber :: Number -> HugeNum
fromNumber n = case parseNumber n of
                    Float -> floatToHugeNum n
                    Scientific -> scientificToHugeNum n
                    Integral -> integralToHugeNum n

{--
addHugeNum :: HugeNum -> HugeNum -> HugeNum
addHugeNum (HugeNum x) (HugeNum y) = HugeNum z where
  smalls :: Array Digit
  smalls = foldl f (pure _zero) (zipWith addDigits (reverse x.small) (reverse y.small))
  bigs = foldl f (pure _zero) (zipWith addDigits (reverse x.big) (reverse y.big))
  f :: Array Digit -> Tuple Digit Digit -> Array Digit
  f xs (Tuple d0 d1) = 

0.1234 + 0.9796
=> [4,3,2,1] +* [6,9,7,9]
=> [(1,0), (1, 2), (0, 9), (1, 0)]
=> [0, (0, 1) + (1, 2), (0, 9), (1, 0)]
=> [0, (1, 3), (0, 9), (1, 0)]
=> [0, 3, (0, 1) + (0, 9), (1, 0)]
=> [0, 3, (1, 0), (1, 0)]
=> [0, 3, 0, (0, 1) + (1, 0)]
=> [0, 3, 0, (1, 1)]
=> [0, 3, 0, 1, 1]
1.234e24
--}

until :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
until p f = go
  where
    go x | p x = x
         | otherwise = go (f x)

isZero :: HugeNum -> Boolean
isZero (HugeNum r) = all (== _zero) r.digits
