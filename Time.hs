module Time where

import Tools

{- Time library -}
newtype Time = Time { unTime :: Integer }

instance Show Time where
    show (Time n)
        | h == 0 && m == 0 = show s
        | h == 0           = show (if m == 60 then 0 else m) ++ ":" ++
                             setLen 2 '0' (show s)
        | otherwise        = show h ++ ":" ++
                             setLen 2 '0' (show (if m == 60 then 0 else m)) ++
                             ":" ++ setLen 2 '0' (show s)
      where
          h = n `div` 3600
          m = n `div` 60
          s = n `mod` 60

instance Num Time where
    Time a + Time b = Time (a + b)
    Time a - Time b = Time (a - b)
    Time a * Time b = Time (a * b)
    abs (Time a)    = Time (abs a)
    signum (Time a) = Time (signum a)
    fromInteger     = Time

instance Semigroup Time where
    Time a <> Time b = Time (a + b)

instance Monoid Time where
    mempty = Time 0

toTime :: Integer -> Integer -> Time
toTime f s = Time (f * 60 + s)

tuTime :: (Integer, Integer) -> Time
tuTime (f, s) = Time (f * 60 + s)