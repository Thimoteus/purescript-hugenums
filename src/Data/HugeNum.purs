module Data.HugeNum
  ( HugeNum()
  , fromNumber
  , toNumber
  , integerPart
  , fractionalPart
  , abs
  , max
  , min
  , neg
  , isNegative
  , isPositive
  , isZero
  , googol
  , pow
  ) where

import Prelude
import Global (readFloat)

import Data.String (toCharArray, fromCharArray, contains)
import Data.Array (span, drop, take, mapMaybe, length, filter, uncons,
                  insertAt, replicate, dropWhile, takeWhile, reverse, (:), zip,
                  null)
import Data.Foldable (foldl, all)
import Data.Maybe.Unsafe (fromJust)
import Data.Int (round, odd)
import Math as Math
import Data.Tuple (Tuple(..), fst, snd)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Data.Digit

-- | ##Type definitions

data Sign = Plus | Minus
type HugeRec = { digits :: Array Digit, decimal :: Int, sign :: Sign }
newtype HugeNum = HugeNum HugeRec

-- | ##Instances

instance arbHugeNum :: Arbitrary HugeNum where
  arbitrary = fromNumber <<< Math.round <<< (* 1000.0) <$> arbitrary

instance eqSign :: Eq Sign where
  eq Plus Plus = true
  eq Minus Minus = true
  eq _ _ = false

instance ordSign :: Ord Sign where
  compare Plus Minus = GT
  compare Minus Plus = LT
  compare _ _ = EQ

timesSign :: Sign -> Sign -> Sign
timesSign Plus Plus = Plus
timesSign Minus Minus = Plus
timesSign _ _ = Minus

instance showHugeNum :: Show HugeNum where
  show = ("HugeNum " ++) <<< toString <<< dropZeroes

instance eqHugeNum :: Eq HugeNum where
  eq x y
    | isZero x && isZero y = true
    | otherwise = strictlyEqual (dropZeroes x) (dropZeroes y)

instance ordHugeNum :: Ord HugeNum where
  compare = compareHugeNum

instance semiringHugeNum :: Semiring HugeNum where
  one = oneHugeNum
  mul = times
  zero = zeroHugeNum
  add = plus

instance ringHugeNum :: Ring HugeNum where
  sub r1 r2 = r1 + neg r2

-- | ## Utility functions

rec :: HugeNum -> HugeRec
rec (HugeNum r) = r

strictlyEqual :: HugeNum -> HugeNum -> Boolean
strictlyEqual (HugeNum r1) (HugeNum r2) =
  r1.decimal == r2.decimal && r1.digits == r2.digits && r1.sign == r2.sign

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
    fractionalDigits = reverse (drop r.decimal r.digits)
    meatyFraction = dropWhile (== _zero) fractionalDigits
    digits = reverse meatyFraction
    z = r { digits = take r.decimal r.digits ++ if null digits then [_zero] else digits }
  dropIntegralZeroes (HugeNum r) = HugeNum z where
    integralPart = take r.decimal r.digits
    zeroes = takeWhile (== _zero) integralPart
    digits = if length integralPart == length zeroes
                then [_zero]
                else drop (length zeroes) integralPart
    decimal = length digits
    z = r { digits = digits ++ drop r.decimal r.digits, decimal = decimal }

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

-- | Specific HugeNums

zeroHugeNum :: HugeNum
zeroHugeNum = HugeNum { digits: [_zero, _zero], decimal: 1 , sign: Plus }

oneHugeNum :: HugeNum
oneHugeNum = HugeNum { digits: [_one, _zero], decimal: 1, sign: Plus }

googol :: HugeNum
googol = HugeNum { digits: _one : replicate 101 _zero, decimal: 101, sign: Plus }

-- | Number -> HugeNum

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
  pos = n >= zero
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

-- | ## Miscellaneous easily-defined functions

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

-- | ## Addition

addPlusPlus :: HugeNum -> HugeNum -> HugeNum
addPlusPlus x y = dropZeroes (HugeNum z) where
  eqv = equivalize { fst: x, snd: y }
  r1 = rec eqv.fst
  r2 = rec eqv.snd
  r = zip (reverse r1.digits) (reverse r2.digits)
  digits'' = foldl digitwiseAdd (Tuple [] _zero) r
  spill = snd digits''
  digits' = fst digits''
  digits = unsafeRemoveFrontZeroes $ spill : digits'
  decimal = adjustDecimalForFrontZeroes (spill : digits') (r1.decimal + 1)
  z = { digits: digits, decimal: decimal, sign: Plus }

digitwiseAdd :: Tuple (Array Digit) Digit -> Tuple Digit Digit -> Tuple (Array Digit) Digit
digitwiseAdd (Tuple xs d) (Tuple t b) =
  let tint = toInt t + toInt d
      bint = toInt b
      summ' = tint + bint
      summ = fromJust $ fromInt if summ' > 9 then summ' - 10 else summ'
      spill = if summ' > 9 then _one else _zero
   in Tuple (summ : xs) spill

addMinusMinus :: HugeNum -> HugeNum -> HugeNum
addMinusMinus x y =
  let z = rec (addPlusPlus x y)
   in HugeNum (z { sign = Minus })

-- | Assumes 0 <= x, 0 <= abs y <= x, y <= 0
addPlusMinus :: HugeNum -> HugeNum -> HugeNum
addPlusMinus x y = (HugeNum z) where
  eqv = equivalize { fst: x, snd: y }
  r2 = rec $ max eqv.fst eqv.snd
  r1 = rec $ min eqv.fst eqv.snd
  r = zip (reverse r2.digits) (reverse r1.digits)
  digits'' = foldl digitwiseSubtract (Tuple [] _zero) r -- check
  integralDigits'' = take r1.decimal $ fst digits''
  fractionalDigits = drop r1.decimal $ fst digits''
  integralDigits' = unsafeRemoveFrontZeroes integralDigits''
  integralDigits = if null integralDigits' then [_zero] else integralDigits' -- check
  decimal = adjustDecimalForFrontZeroes (fst digits'') r1.decimal
  digits = integralDigits ++ fractionalDigits
  z = { digits: digits, decimal: decimal, sign: Plus }

digitwiseSubtract :: Tuple (Array Digit) Digit -> Tuple Digit Digit -> Tuple (Array Digit) Digit
digitwiseSubtract (Tuple xs d) (Tuple t b) =
  let tint = toInt t - toInt d
      bint = toInt b
      diff' = tint - bint
      diff = fromJust $ fromInt if diff' < 0 then diff' + 10 else diff'
      spill = if diff' < 0 then _one else _zero
   in Tuple (diff : xs) spill

unsafeRemoveFrontZeroes :: Array Digit -> Array Digit
unsafeRemoveFrontZeroes = dropWhile (== _zero)

adjustDecimalForFrontZeroes :: Array Digit -> Int -> Int
adjustDecimalForFrontZeroes xs oldDec =
  let newDigits' = unsafeRemoveFrontZeroes $ take oldDec xs
      newDigits = if null newDigits' then [_zero] else newDigits'
   in length newDigits

plus :: HugeNum -> HugeNum -> HugeNum
plus x y
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
           else neg (addPlusMinus greaterMag lesserMag)

subHugeNum :: HugeNum -> HugeNum -> HugeNum
subHugeNum x y = plus x (neg y)

-- | ## Multiplication

-- | For any integral x with n digits in base B, x = x1 * B^m + x0 for all m and
-- | x0 < B^m.
type KRep = { exp :: Int, coeff :: Array Digit, const :: HugeNum }

-- | Drop the _zeroes on the tail.
takeMeatyParts :: Array Digit -> Array Digit
takeMeatyParts arr =
  reverse (dropWhile (== _zero) (reverse arr))

-- | Turn a `KRep` into a `HugeNum`.
fromKRep :: KRep -> HugeNum
fromKRep k = z where
  bm = { sign: Plus, digits: _one : replicate (k.exp + 1) _zero, decimal: k.exp + 1 }
  prod = k.coeff ++ drop 1 bm.digits
  leftSummand = HugeNum { digits: prod, sign: Plus, decimal: bm.decimal + length k.coeff - 1 }
  z = plus leftSummand k.const

-- | Turn a `HugeNum` into a `KRep`, given an exponent m for B^m.
toKRep :: Int -> HugeNum -> KRep
toKRep exp h@(HugeNum r) = z where
  bm = _one : replicate exp _zero
  coeff = take (r.decimal - exp) r.digits
  prod = coeff ++ drop 1 bm
  leftSummand = arrayToHugeNum prod
  k = plus h (neg leftSummand)
  z = { exp: exp, coeff: coeff, const: k }

-- | Takes two HugeNums and calculates a suitable exponent m in B^m.
getPowForKRep :: HugeNum -> HugeNum -> Int
getPowForKRep x y = (`sub` 1) $ _.decimal $ rec $ min (abs x) (abs y)

-- | Turns an array of digits into an integral HugeNum.
arrayToHugeNum :: Array Digit -> HugeNum
arrayToHugeNum xs =
  HugeNum { sign: Plus, digits: xs ++ [_zero], decimal: length xs }

-- | Test for whether we can reach the base case of recursive multiplication.
smallEnough :: HugeNum -> Boolean
smallEnough (HugeNum { digits = [_, _], decimal = 1 }) = true
smallEnough _ = false

-- | Multiplying by a power of ten is easy. All we have to do is append _zeroes!
timesPowOfTen :: Int -> HugeNum -> HugeNum
timesPowOfTen n (HugeNum r) = z where
  newDecimal = r.decimal + n
  newZeroesLength = if newDecimal >= length (takeMeatyParts r.digits)
                       then newDecimal - length (takeMeatyParts r.digits)
                       else 0
  newZeroes = replicate newZeroesLength _zero
  z = HugeNum { digits: r.digits ++ newZeroes, sign: r.sign, decimal: newDecimal }

-- | Karatsuba multiplication.
times :: HugeNum -> HugeNum -> HugeNum
times r1 r2
  | timesSign (_.sign $ rec r1) (_.sign $ rec r2) == Minus = neg (times (abs r1) (abs r2))
  | not (trivialFraction r1) || not (trivialFraction r2) =
    adjustDecimalForTriviality r1 r2 $ times (makeHugeInteger r1) (makeHugeInteger r2)
  | smallEnough r1 = multSmallNum r1 r2
  | smallEnough r2 = multSmallNum r2 r1
  | otherwise = z where
    exp = getPowForKRep r1 r2
    k1 = toKRep exp r1
    k2 = toKRep exp r2
    x0 = k1.const
    y0 = k2.const
    x1 = k1.coeff
    y1 = k2.coeff
    z0 = times x0 y0
    z2 = times (arrayToHugeNum x1) (arrayToHugeNum y1)
    z1 = let leftFactor = plus (arrayToHugeNum x1) x0
             rightFactor = plus (arrayToHugeNum y1) y0
             prod = times leftFactor rightFactor
          in plus (plus prod (neg z2)) (neg z0)
    z2B2m = timesPowOfTen (2 * exp) z2
    z1Bm = timesPowOfTen exp z1
    z = plus (plus z2B2m z1Bm) z0

-- | "Scalar" multiplication by an Int.
scale :: Int -> HugeNum -> HugeNum
scale = go zeroHugeNum where
  go x 0 _ = x
  go x n k = go (plus x k) (n - 1) k

-- | The base case of recursive multiplication
multSmallNum :: HugeNum -> HugeNum -> HugeNum
multSmallNum (HugeNum { digits = [x, ze] }) r2
  | x == _zero = zeroHugeNum
  | otherwise = scale (toInt x) r2

-- | Count how much information the fractional part of a HugeNum holds.
meatyDecimals :: HugeNum -> Int
meatyDecimals (HugeNum r) =
  let decimals = reverse $ drop r.decimal r.digits
      meaty = dropWhile (== _zero) decimals
   in length meaty

isHugeInteger :: HugeNum -> Boolean
isHugeInteger (HugeNum r) = all (== _zero) $ drop r.decimal r.digits

-- | Moves the decimal place in a HugeNum so it has a trivial fractional part.
makeHugeInteger :: HugeNum -> HugeNum
makeHugeInteger r = if isHugeInteger r then r else makeHugeInteger' r

makeHugeInteger' :: HugeNum -> HugeNum
makeHugeInteger' (HugeNum r) = HugeNum z where
  digits' = reverse r.digits
  meaty = dropWhile (== _zero) digits'
  decimal = length meaty
  digits = reverse meaty ++ [_zero]
  sign = r.sign
  z = { digits: digits, decimal: decimal, sign: sign }

-- | Test for whether a HugeNum has any non-_zero digits in its fractional part.
trivialFraction :: HugeNum -> Boolean
trivialFraction (HugeNum r) =
  let decimals = reverse $ drop r.decimal r.digits
      meaty = dropWhile (== _zero) decimals
   in null meaty

-- | When multiplying two HugeNums and one has a nontrivial fractional part,
-- | we first turn them into integral HugeNums, then calculate where the
-- | decimal should be.
adjustDecimalForTriviality :: HugeNum -> HugeNum -> HugeNum -> HugeNum
adjustDecimalForTriviality h1 h2 (HugeNum r3) = HugeNum r where
  digits = take (length r3.digits - 1) r3.digits
  decimalMod = meatyDecimals h1 + meatyDecimals h2
  decimal = length $ drop decimalMod $ reverse digits
  sign = Plus
  r = { digits: digits, decimal: decimal, sign: sign }

-- | Raise a HugeNum to an integer power.
pow :: HugeNum -> Int -> HugeNum
pow r 1 = r
pow r n =
  let c = r * r
      ans = pow c (n / 2)
   in if odd n
         then r * ans
         else ans
