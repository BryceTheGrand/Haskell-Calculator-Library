module Probability where

{- Statistics and Probability -}
sort :: Ord a => [a] -> [a]
sort xs = sortAll (map (:[]) xs)
  where
    sortAll []  = []
    sortAll [x] = x
    sortAll (x:y:xs) = sortAll (merge x y : xs)
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

mean :: (Fractional a, Num a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

median :: (Fractional a, Num a, Ord a) => [a] -> a
median xs
    | length xs `mod` 2 == 1 = sort xs !! ((len + 1) `div` 2 - 1)
    | otherwise              = mean [sort xs !! (len `div` 2 - 1), sort xs !! (len `div` 2)]
  where
      len = length xs

variance :: (Fractional a, Num a, Floating a) => [a] -> a
variance xs = sum (map (\ x -> (x - u) ^ 2) xs) / fromIntegral (length xs - 1)
  where
    u = mean xs

popVariance :: (Fractional a, Num a, Floating a) => [a] -> a
popVariance xs = sum (map (\ x -> (x - u) ^ 2) xs) / fromIntegral (length xs)
  where
    u = mean xs

std :: (Fractional a, Num a, Floating a) => [a] -> a
std = sqrt . variance

popStd :: (Fractional a, Num a, Floating a) => [a] -> a
popStd = sqrt . popVariance

factorial :: Integer -> Integer
factorial 0 = 1
factorial n
    | n < 0     = error "The value of n must be a positive integer value."
    | otherwise = n * factorial (n - 1)

p :: Integer -> Integer -> Integer
p n r = factorial n `div` factorial (n - r)

c :: Integer -> Integer -> Integer
c n r = factorial n `div` (factorial r * factorial (n - r))