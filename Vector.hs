module Vector where

data Vector a = Vector { x :: a
                       , y :: a
                       , z :: a }

instance (Show a, Ord a, Num a) => Show (Vector a) where
    show (Vector a b c) = show a ++ (if b >= 0 then "i + " ++ show b else "i - " ++ show (-b)) ++
                          (if c >= 0 then "j + " ++ show c else "j - " ++ show (-c)) ++ "k"

instance Num a => Num (Vector a) where
    Vector a b c + Vector d e f = Vector (a + d) (b + e) (c + f)
    negate (Vector a b c)       = Vector (-a) (-b) (-c)
    Vector a b c * Vector d e f = Vector (b * f - c * e) (c * d - a * f) (a * e - b * d)
    abs (Vector a b c)          = Vector (abs a) (abs b) (abs c)
    signum (Vector a b c)       = Vector (signum a) (signum b) (signum c)
    fromInteger n               = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n)

instance Functor Vector where
    fmap f (Vector a b c) = Vector (f a) (f b) (f c)

vDot :: Num a => Vector a -> Vector a -> a
vDot (Vector a b c) (Vector d e f) = a * d + b * e + c * f

vMag :: (Num a, Floating a) => Vector a -> a
vMag (Vector a b c) = sqrt $ a ^ 2 + b ^ 2 + c ^ 2

proj :: (Num a, Floating a) => Vector a -> Vector a -> Vector a
proj p b = fmap (* (vDot p b / (vMag b ^ 2))) b