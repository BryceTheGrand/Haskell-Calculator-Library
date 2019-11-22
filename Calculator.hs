{- Bryce's supreme calculator -}
{-
Creator: Bryce Elvin
Institution: Ontario Tech University (University of Ontario Institute of
             Technology)
Creation Date: 11/21/2019
Description:
    Essentially a large group of calculator functions for quick calculations
    without needing to use multiple different websites. In other words, a hub
    world for high-level math.
-}

-- Get engineering notation of a decimal number
eng :: (Num a, Show a) => a -> String
eng x
    | 'e' `elem` show x = let power = read (tail (dropWhile (/='e') (show x))) :: Int in
                            case power of
                                -2  -> show (x * 1000)       ++ "m"
                                -3  -> show (x * 1000)       ++ "m"
                                -4  -> show (x * 1000)       ++ "m"
                                -5  -> show (x * 1000000)    ++ "u"
                                -6  -> show (x * 1000000)    ++ "u"
                                -7  -> show (x * 1000000)    ++ "u"
                                -8  -> show (x * 1000000000) ++ "n"
                                -9  -> show (x * 1000000000) ++ "n"
                                -10 -> show (x * 1000000000) ++ "n"
                                _   -> show x
    | otherwise = show x

-- Group a list into chunks of n size
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

-- Drop the nth item from a list
dropN :: Int -> [a] -> [a]
dropN _ []     = []
dropN 0 (_:xs) = xs
dropN n (x:xs) = x : dropN (n - 1) xs

{-
Complex Numbers Package Section:
    Used to compute using complex numbers.
-}

{- Complex datatype. This has two subtypes: Rect and Polar.
Rect datatype takes a real part and an imaginary part,
Polar datatype takes a magnitude and an angle.           -}
data Complex = Rect  { real :: Double, imag :: Double }
             | Polar { mag  :: Double, ang  :: Double }

-- For simplicity
type Degrees = Double
type Radians = Double

-- Convert radians to degrees
deg :: Radians -> Degrees
deg r = 360 * r / (2 * pi)

-- Convert degrees to radians
rad :: Degrees -> Radians
rad d = 2 * pi * d / 360

-- Take a complex number and convert it to its counterpart...
-- i.e. Polar -> Rect, Rect -> Polar
convert :: Complex -> Complex
convert (Rect x y)  = Polar (sqrt $ x*x + y*y) (atan (y / x))
convert (Polar m a) = Rect (m * cos a) (m * sin a)

-- Take a polar complex number and return its reduced form (0 <= angle <= 2*pi)
reduce :: Complex -> Complex
reduce comp@(Polar m a)
    | a == (-180)    = Polar m (-a)
    | a > 2 * pi     = reduce (Polar m (a - 2 * pi))
    | a < (- 2 * pi) = reduce (Polar m (a + 2 * pi))
    | otherwise      = comp
reduce comp@(Rect _ _) = comp

-- Convert a complex number to its RMS equivalent
rms :: Complex -> Complex
rms = (/ Rect (sqrt 2) 0)

-- Convert complex number from RMS to max form
unRMS :: Complex -> Complex
unRMS = (* Rect (sqrt 2) 0)

-- Return the complex conjugate of the number
conj :: Complex -> Complex
conj (Rect a b)  = Rect a (-b)
conj (Polar m a) = Polar m (-a)

-- Next lines are all typeclass declarations for Complex
instance Eq Complex where
    Rect a b == Rect c d           = a == c && b == d
    a@(Polar _ _) == b@(Polar _ _) = convert a == convert b
    a@(Polar _ _) == b@(Rect _ _)  = convert a == b
    a@(Rect _ _) == b@(Polar _ _)  = a == convert b

instance Show Complex where
    show (Rect x y)
        | y >= 0    = show x ++ " + " ++ show y ++ "i"
        | otherwise = show x ++ " - " ++ show (-y) ++ "i"
    show (Polar m a) = show m ++ " < " ++ show (deg a) ++ " degrees"

instance Num Complex where
    Rect a b + Rect c d           = Rect (a + c) (b + d)
    x@(Polar _ _) + y@(Polar _ _) = convert $ convert x + convert y
    x@(Polar _ _) + y@(Rect _ _)  = convert $ convert x + y
    x@(Rect _ _) + y@(Polar _ _)  = x + convert y
    negate (Rect a b)             = Rect (-a) (-b)
    negate (Polar m a)            = reduce $ Polar m (a + pi)
    0 * _                         = 0
    _ * 0                         = 0
    Polar a b * Polar c d         = reduce $ Polar (a * c) (b + d)
    x@(Rect _ _) * y@(Polar _ _)  = convert $ convert x * y
    x@(Polar _ _) * y@(Rect _ _)  = x * convert y
    Rect a b * Rect c d           = Rect (a*c - b*d) (a*d + b*c)
    abs (Rect a b)                = Rect (abs a) (abs b)
    abs x@(Polar _ _)             = convert $ abs $ convert x
    fromInteger n                 = Rect (fromIntegral n) 0
    signum (Rect a b)             = Rect (signum a) (signum b)
    signum (Polar m a)            = Polar (signum m) (signum a)

instance Fractional Complex where
    Rect a b / Rect c d           = Rect ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))
    Polar x a / Polar y b         = Polar (x / y) (a - b)
    Rect a b / polar@(Polar _ _)  = Rect a b / convert polar 
    Polar a b / rect@(Rect _ _)   = Polar a b / convert rect
    fromRational _                = Polar 0 0

{-
Matrix datatype section.
-}

type Row a    = [a]
type Column a = [a]

-- Matrix datatype (stores a list of rows)
newtype Matrix a = Matrix { unMatrix :: [Row a] }

-- Typeclass declarations for Matrix
instance Show a => Show (Matrix a) where
    show (Matrix [])     = ""
    show (Matrix [x])    = show x
    show (Matrix (x:xs)) = show x ++ "\n" ++ show (Matrix xs)

instance Num a => Num (Matrix a) where
    Matrix xs + Matrix ys = Matrix $ zipWith (zipWith (+)) xs ys
    Matrix xs - Matrix ys = Matrix $ zipWith (zipWith (-)) xs ys
    m1@(Matrix xs) * m2@(Matrix ys) = Matrix
                                    $ group (length $ head xs)
                                    $ mult (getCombos m1 m2) xs ys
    abs (Matrix xs)   = Matrix $ map (map abs) xs
    signum (Matrix _) = error "Cannot reduce undetermined matrix"
    fromInteger n     = Matrix [[fromIntegral n]]

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
solve :: (Num a, Fractional a) => Int -> Matrix a -> Matrix a -> a
solve n mat@(Matrix _) sol@(Matrix _) = det mat' / det mat
  where
    mat' = transpose
         $ trade n (transpose mat) (head $ unMatrix $ transpose sol)

-- Get the variable vector of a matrix given a solution vector
solveAll :: (Show a, Fractional a) => Matrix a -> Matrix a -> [a]
solveAll xs as = [ solve n xs as | n <- [0..length (head (unMatrix xs)) - 1] ]