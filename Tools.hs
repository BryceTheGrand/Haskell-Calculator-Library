module Tools where

-- Get nth root of a number
root :: Floating a => a -> a -> a
x `root` n = x ** (1 / n)

setLen :: Int -> Char -> String -> String
setLen n c str
    | length str >= n = str
    | otherwise       = setLen n c (c:str)

-- Get engineering notation of a decimal number
eng :: (Num a, Show a, Fractional a) => a -> String
eng x
    | 'e' `elem` show x = let power = read (tail (dropWhile (/='e') (show x))) :: Int in
                            case power of
                                9   -> show (x / 1000000)    ++ "M"
                                8   -> show (x / 1000000)    ++ "M"
                                7   -> show (x / 1000000)    ++ "M"
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