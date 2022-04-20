toDigits :: Integer -> [Integer]
toDigits x
 |x <= 0 = []
 |otherwise = (toDigits (x `div` 10)) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
 |x <= 0 = []
 |otherwise = [x `mod` 10] ++ (toDigitsRev (x `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]  
doubleEveryOther (x:y:ys) = x : (2*y) : doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
 |x >= 10 = div x 10 + mod x 10 + sumDigits xs
 |x < 10 = x + sumDigits xs
 |otherwise = sumDigits xs

validate :: Integer -> Bool
validate x
 |(sumDigits(doubleEveryOther (toDigitsRev x))) `mod` 10 == 0 = True
 |otherwise = False