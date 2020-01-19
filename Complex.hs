module Complex where

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
        | x == 0 && y == 0  = "0"
        | x == 0 && y == -1 = "-i"
        | x == 0 && y == 1  = "i"
        | x == 0            = show y ++ "i"
        | y == 0            = show x
        | y == -1           = show x ++ " - i"
        | y == 1            = show x ++ " + i"
        | y >= 0            = show x ++ " + " ++ show y ++ "i"
        | otherwise         = show x ++ " - " ++ show (-y) ++ "i"
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