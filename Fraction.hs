module Fraction where

import Data.Ratio (numerator, denominator)

{-
Redone rational numbers section
-}
data Fraction = Frac { num :: Integer
                     , den :: Integer }

instance Show Fraction where
    show (Frac a b) = show' (reduceFrac $ Frac a b)
      where
        show' (Frac c d)
            | c == 0 && b /= 0 = "0"
            | c == 1 && b == 1 = "1"
            | b == 1           = show c
            | b == -1          = show (-c)
            | b < 0            = show (-c) ++ " / " ++ show (-d)
            | b > 0            = show c ++ " / " ++ show d
            | otherwise        = "NaN"

instance Num Fraction where
    Frac a b + Frac c d = reduceFrac $ Frac (a*d + b*c) (b*d)
    negate (Frac a b)   = reduceFrac $ Frac (-a) b
    Frac a b * Frac c d = reduceFrac $ Frac (a*c) (b*d)
    abs (Frac a b)      = reduceFrac $ Frac (abs a) (abs b)
    signum (Frac a b)   = reduceFrac $ Frac (signum (a `div` b)) 1
    fromInteger n       = reduceFrac $ Frac n 1

instance Fractional Fraction where
    recip (Frac a b) = reduceFrac $ Frac b a
    fromRational n   = reduceFrac $ Frac (numerator n) (denominator n)

instance Eq Fraction where
    x == y = num a == num b && den a == den b
      where
        (a, b) = (reduceFrac x, reduceFrac y)

instance Ord Fraction where
    x < y  = x - y > 0
    x <= y = x - y >= 0

instance Real Fraction where
    toRational (Frac a b) = toRational a / toRational b

instance Enum Fraction where
    toEnum n            = Frac (fromIntegral n) 1
    fromEnum (Frac a b) = fromIntegral (a `div` b)

instance Integral Fraction where
    toInteger (Frac a b)          = a `div` b
    quotRem (Frac a b) (Frac c d) = (Frac (a*c + b*d) 1, Frac (b*c) 1)

instance Read Fraction where
    readsPrec _ = readFrac

reduceFrac :: Fraction -> Fraction
reduceFrac (Frac a b) = Frac (a `div` x) (b `div` x)
  where
    x = gcd a b

readFrac :: String -> [(Fraction, String)]
readFrac str = [(reduceFrac $ Frac (read $ takeWhile (/='/') str)
                (read $ tail $ dropWhile (/='/') str), "")]
