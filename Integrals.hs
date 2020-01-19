module Integrals where

{-
Integration Section
-}
leftInt :: (Double -> Double) -> Double -> Double -> Int -> Double
leftInt func lower upper nRec = leftInt' func lower upper nRec
  where
    width = (upper - lower) / fromIntegral nRec
    leftInt' f low upp n
        | low >= upp = 0
        | otherwise  = f low * width + leftInt' f (low + width) upp n

rightInt :: (Double -> Double) -> Double -> Double -> Int -> Double
rightInt func lower upper nRec = rightInt' func lower upper nRec
  where
    width = (upper - lower) / fromIntegral nRec
    rightInt' f low upp n
        | low + width >= upp = 0
        | otherwise          = f (low + width) * width + rightInt' f (low + width) upp n

simpsons :: (Double -> Double) -> Double -> Double -> Int -> Double
simpsons func lower upper nRec = (width / 3) * (func lower + simpsons' xVals)
  where
    width = (upper - lower) / fromIntegral nRec
    xVals = tail [lower,lower + width..upper]
    simpsons' []  = 0
    simpsons' [x] = func x
    simpsons' (x:xs)
        | length xs `mod` 2 == 1 = 4 * func x + simpsons' xs
        | otherwise              = 2 * func x + simpsons' xs
