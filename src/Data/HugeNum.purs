module Data.HugeNum
  ( HugeNum()
  , fromNumber
  , toNumber
  , lessPrecise
  , integerPart
  , fractionalPart
  , abs
  , max
  , min
  , neg
  , isNegative
  , isPositive
  , isZero
  , addHugeNum
  , addPlusMinus
  , multSmallNum
  , scale
  ) where

import Prelude
import Global (readFloat)

import Data.String (toCharArray, fromCharArray, contains)
import Data.Array (span, drop, take, mapMaybe, zipWith, length, filter, uncons,
                  insertAt, replicate, dropWhile, takeWhile, reverse, (:), zip,
                  null)
import Data.Foldable (foldl, all)
import Data.Maybe.Unsafe (fromJust)
import Data.Int (round)
import Data.Tuple (Tuple(..), fst, snd)
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
  show = ("HugeNum " ++) <<< toString <<< dropZeroes

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
  | r1.sign < r2.sign = LT
  | r1.sign > r2.sign = GT
  | r1.decimal > r2.decimal = GT
  | r1.decimal < r2.decimal = LT
  | x == y = EQ
  | otherwise = z where
    dec = r1.decimal
    r = equivalize { fst: x, snd: y }
    s = rec r.fst
    m = rec r.snd
    x' = if x == r.fst then s else m
    y' = if y == r.fst then s else m
    z = case compare (take dec r1.digits) (take dec r2.digits) of
             EQ -> compare (drop dec x'.digits) (drop dec y'.digits)
             x -> x

dropZeroes :: HugeNum -> HugeNum
dropZeroes = dropIntegralZeroes <<< dropFractionalZeroes where
  dropFractionalZeroes (HugeNum r) = HugeNum z where
    iPutMyThingDown = reverse (drop r.decimal r.digits)
    flipIt = dropWhile (== _zero) iPutMyThingDown
    andReverseIt = reverse flipIt
    z = r { digits = take r.decimal r.digits ++ if null andReverseIt then [_zero] else andReverseIt }
  dropIntegralZeroes (HugeNum r) = HugeNum z where
    decimalMod = length (takeWhile (== _zero) r.digits)
    digits = drop decimalMod r.digits
    decimal = r.decimal - decimalMod
    z = r { digits = digits, decimal = decimal }

equivalize :: { fst:: HugeNum, snd :: HugeNum } -> { fst :: HugeNum, snd :: HugeNum }
equivalize = integralize <<< fractionalize where
  fractionalize { fst = x@(HugeNum r1), snd = y@(HugeNum r2) }
    | length (drop r1.decimal r1.digits) == length (drop r2.decimal r2.digits) = { fst: x, snd: y }
    | otherwise = z where
      test = x `lessPrecise` y
      lesser = if test then r1 else r2
      greater = if test then r2 else r1
      lesserDecimal = length (drop greater.decimal greater.digits) - length (drop lesser.decimal lesser.digits)
      zeroes = replicate lesserDecimal _zero
      lesser' = lesser { digits = lesser.digits ++ zeroes }
      z = { fst: HugeNum lesser', snd: HugeNum greater }
  integralize { fst = x@(HugeNum r1), snd = y@(HugeNum r2) }
    | length (take r1.decimal r1.digits) == length (take r2.decimal r2.digits) = { fst: x, snd: y }
    | otherwise = z where
      lesser = rec (min x y)
      greater = rec (max x y)
      zeroesLength = length (take greater.decimal greater.digits) - length (take lesser.decimal lesser.digits)
      zeroes = replicate zeroesLength _zero
      lesser' = lesser { digits = zeroes ++ lesser.digits, decimal = greater.decimal }
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
   in fromCharArray (sign ++ numray)

zeroHugeNum :: HugeNum
zeroHugeNum = HugeNum { digits: [_zero, _zero], decimal: 1 , sign: Plus }

oneHugeNum :: HugeNum
oneHugeNum = HugeNum { digits: [_one, _one], decimal: 1, sign: Plus }

data NumberStyle = Float | Integral | Scientific
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

parseScientific :: Number -> { exponent :: Int, expSign :: Sign, base :: Array Char, sign :: Sign }
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

abs :: HugeNum -> HugeNum
abs (HugeNum r) = HugeNum (r { sign = Plus })

isNegative :: HugeNum -> Boolean
isNegative (HugeNum { sign = Minus }) = true
isNegative _ = false

isPositive :: HugeNum -> Boolean
isPositive (HugeNum { sign = Plus }) = true
isPositive _ = false

isZero :: HugeNum -> Boolean
isZero (HugeNum r) = all (== _zero) r.digits

min :: HugeNum -> HugeNum -> HugeNum
min x y = if x < y then x else y

max :: HugeNum -> HugeNum -> HugeNum
max x y = if x > y then x else y

neg :: HugeNum -> HugeNum
neg (HugeNum r@{ sign = Minus }) = HugeNum (r { sign = Plus })
neg (HugeNum r) = HugeNum (r { sign = Minus })

addPlusPlus :: HugeNum -> HugeNum -> HugeNum
addPlusPlus x y = HugeNum z where
  eqv = equivalize { fst: x, snd: y }
  r1 = rec eqv.fst
  r2 = rec eqv.snd
  r = zipWith addDigits (reverse r1.digits) (reverse r2.digits)
  digits'' = foldl f (Tuple [] _zero) r
  f :: Tuple (Array Digit) Digit -> Tuple Digit Digit -> Tuple (Array Digit) Digit
  f (Tuple xs d) (Tuple d1 d2) = Tuple (fromJust (fromInt $ toInt d + toInt d2) : xs) d1
  digits' = fst digits''
  spillover = snd digits''
  digitsDecimal = if toInt spillover == 0
                     then Tuple digits' r1.decimal
                     else Tuple (spillover : digits') (r1.decimal + 1)
  digits = fst digitsDecimal
  decimal = snd digitsDecimal
  z = { digits: digits, decimal: decimal, sign: Plus }

addMinusMinus :: HugeNum -> HugeNum -> HugeNum
addMinusMinus x y =
  let z = rec (addPlusPlus x y)
   in HugeNum (z { sign = Minus })

addPlusMinus :: HugeNum -> HugeNum -> HugeNum
addPlusMinus x y = HugeNum z where
  eqv = equivalize { fst: x, snd: y }
  r2 = rec $ max eqv.fst eqv.snd
  r1 = rec $ min eqv.fst eqv.snd
  r = zip (reverse r2.digits) (reverse r1.digits)
  digits' = foldl f (Tuple [] _zero) r
  f :: Tuple (Array Digit) Digit -> Tuple Digit Digit -> Tuple (Array Digit) Digit
  f (Tuple xs d) (Tuple t b) =
    let tint = toInt t - toInt d
        bint = toInt b
        diff' = tint - bint
        diff = fromJust $ fromInt if diff' < 0 then diff' + 10 else diff'
        spill = if diff' < 0 then _one else _zero
     in Tuple (diff : xs) spill
  digits = fst digits'
  decimal = r1.decimal
  z = { digits: digits, decimal: decimal, sign: Plus }

addHugeNum :: HugeNum -> HugeNum -> HugeNum
addHugeNum x y
  | isZero x = y
  | isZero y = x
  | isPositive x && isPositive y = addPlusPlus x y
  | isNegative x && isNegative y = addMinusMinus x y
  | otherwise = z where
    greaterMag = max (abs x) (abs y)
    lesserMag = min (abs x) (abs y)
    greater = max x y
    lesser = min x y
    z = if greater == greaterMag
           then addPlusMinus greater lesser
           else neg (addPlusMinus (abs greater) lesser)

subHugeNum :: HugeNum -> HugeNum -> HugeNum
subHugeNum x y = addHugeNum x (neg y)

scale :: Int -> HugeNum -> HugeNum
scale = go zeroHugeNum where
  go x 0 _ = x
  go x n k = go (addHugeNum x k) (n - 1) k

--multHugeNum :: HugeNum -> HugeNum -> HugeNum
--multHugeNum x y = z where

multSmallNum :: HugeNum -> HugeNum -> HugeNum
multSmallNum (HugeNum { digits = [x, _] }) r2
  | x == _zero = zeroHugeNum
  | otherwise = scale (toInt x) r2
