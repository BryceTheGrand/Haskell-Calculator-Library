module Matrix where

import Tools
import Data.List(find)

{-
Matrix datatype section.
-}

type Row a    = [a]
type Column a = [a]

-- Matrix datatype (stores a list of rows)
newtype Matrix a = Matrix { unMatrix :: [Row a] }

-- Typeclass declarations for Matrix
instance Show a => Show (Matrix a) where
    show (Matrix [])     = "[]"
    show (Matrix [x])    = show x
    show mat@(Matrix _) = showLines
                        $ unMatrix
                        $ makeLength (maxLength mat) <$> fmap show mat
      where
        showLines []     = ""
        showLines [x]    = filter (/= '"') (show x)
        showLines (x:xs) = filter (/= '"') (show x) ++ "\n" ++ showLines xs

instance Num a => Num (Matrix a) where
    Matrix xs + Matrix ys = Matrix $ zipWith (zipWith (+)) xs ys
    Matrix xs - Matrix ys = Matrix $ zipWith (zipWith (-)) xs ys
    m1@(Matrix xs) * m2@(Matrix ys) = Matrix
                                    $ group (length $ head xs)
                                    $ mult (getCombos m1 m2) xs ys
    abs (Matrix xs)   = Matrix $ map (map abs) xs
    signum (Matrix _) = error "Cannot reduce undetermined matrix"
    fromInteger n     = Matrix [[fromIntegral n]]

instance Functor Matrix where
    fmap f (Matrix xs) = Matrix (map (map f) xs)

getIndex :: Int -> Int -> Matrix a -> a
getIndex i j m = unMatrix m !! i !! j

makeLength :: Int -> String -> String
makeLength n x
    | length x < n = makeLength n (' ' : x)
    | otherwise    = x

maxLength :: Show a => Matrix a -> Int
maxLength (Matrix xs) = maxLengthL xs

maxLengthL :: Show a => [[a]] -> Int
maxLengthL = largest . map maxLength'
  where
    maxLength' = foldl (\x y -> if length x < length (show y) then show y else x) ""
    largest = length . foldl (\x y -> if length x < length y then y else x) ""

-- Get the nth column of a matrix
getColumn :: Int -> Matrix a -> Column a
getColumn _ (Matrix [])     = []
getColumn n (Matrix [x])    = [x !! n]
getColumn n (Matrix (x:xs)) = x !! n : getColumn n (Matrix xs)

-- Get the nth row of a matrix
getRow :: Int -> Matrix a -> Row a
getRow _ (Matrix []) = []
getRow n (Matrix xs) = xs !! n

-- Transpose a matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix xs') = Matrix (transpose' 0 xs')
  where
    transpose' _ [] = []
    transpose' n (x:xs)
        | n >= length x = []
        | otherwise     = getColumn n (Matrix (x:xs)) : transpose' (n + 1) (x:xs)

-- Get an nth order identity matrix
identity :: (Num a, Integral a) => Int -> Matrix a
identity n = Matrix $ identity' n
  where
    identity' 0 = []
    identity' x = (replicate (n - x) 0 ++ [1] ++ replicate (x - 1) 0) : identity' (x - 1)

-- Matrix multiplication function for use in Num instance
mult :: Num a => [(Int, Int)] -> [[a]] -> [[a]] -> [a]
mult [] _ _ = []
mult ((n, m):ns) xs ys = dot (xs !! n) (ys !! m) : mult ns xs ys

-- Get a list of combos (used for matrix multiplication)
getCombos :: Matrix a -> Matrix a -> [(Int, Int)]
getCombos (Matrix (x:_)) (Matrix ys)
    | length x == length ys = [ (a, b)
                              | a <- [0..length x - 1]
                              , b <- [0..length ys - 1] ]
getCombos _ _ = error "Invalid matrices"

-- Get the dot product of two rows (or column vectors)
dot :: Num a => Row a -> Row a -> a
dot xs ys
    | length xs /= length ys = error "Dimension error"
    | otherwise              = sum $ zipWith (*) xs ys

-- Get the determinant of a matrix
det :: Num a => Matrix a -> a
det (Matrix ls) = det' 0 ls
  where
    det' _ [[x]] = x
    det' n xs
        | n < length (head xs) = (-1) ^ n * (head xs !! n)
                                 * det' 0 (map (dropN n) $ dropN 0 xs)
                                 + det' (n + 1) xs
        | otherwise            = 0

-- Swap out nth column for another column in a matrix
trade :: Int -> Matrix a -> Column a -> Matrix a
trade n' (Matrix xs') ys' = Matrix $ trade' n' xs' ys'
  where
    trade' _ [] _      = error "Trade incomplete"
    trade' 0 (_:xs) ys = ys : xs
    trade' n (x:xs) ys = x : trade' (n - 1) xs ys

-- Solve for a variable in a matrix given a solution vector
solve :: (Num a, Fractional a) => Int -> Matrix a -> Column a -> a
solve n mat@(Matrix _) xs = det mat' / det mat
  where
    mat' = transpose
         $ trade n (transpose mat) xs

addColumn :: [a] -> Matrix a -> Matrix a
addColumn as (Matrix xs') = Matrix $ addColumn' xs' as
  where
    addColumn' [] _ = []
    addColumn' _ [] = []
    addColumn' (x:xs) (y:ys) = (x ++ [y]) : addColumn' xs ys

-- Get the variable vector of a matrix given a solution vector
solveAll :: (Show a, Fractional a) => Matrix a -> Column a -> Matrix a
solveAll xs as = Matrix [ [solve n xs as] | n <- [0..length (head (unMatrix xs)) - 1] ]

solveAllRREF :: (Show a, Fractional a, Eq a) => Matrix a -> Column a -> Matrix a
solveAllRREF (Matrix []) _ = error "Cannot perform row reduction on an empty matrix"
solveAllRREF mat@(Matrix (x:_)) as = Matrix $ map (drop (length x))
                                    $ unMatrix $ rref $ addColumn as mat

rref :: (Eq a, Fractional a) => Matrix a -> Matrix a
rref (Matrix xs) = Matrix $ rref' xs

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

-- Stolen RREF function from rosettacode.org
rref' :: (Eq a, Fractional a) => [[a]] -> [[a]]
rref' n = f n 0 [0 .. rows - 1]
  where rows = length n
        cols = length $ head n
        f m _    []              = m
        f m lead (r : rs)
            | isNothing indices = m
            | otherwise          = f m' (lead' + 1) rs
          where indices = find p l
                p (col, row) = m !! row !! col /= 0
                l = [ (col, row) |
                      col <- [lead .. cols - 1],
                      row <- [r .. rows - 1] ]
                Just (lead', i) = indices
                newRow = map (/ m !! i !! lead') $ m !! i
                m' = zipWith g [0..] $
                     replace r newRow $
                     replace i (m !! r) m
                g x row
                    | x == r    = row
                    | otherwise = zipWith h newRow row
                  where h = subtract . (* row !! lead')
 
replace :: Int -> a -> [a] -> [a]
{- Replaces the element at the given index. -}
replace n e l = a ++ e : b
  where (a, _ : b) = splitAt n l
