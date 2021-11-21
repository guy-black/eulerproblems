-- take all sum all even fibonacci numbers below 4million

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

evenFibs :: [Integer]
evenFibs = filter even fibs

evenFibsUntil :: Integer -> [Integer]
evenFibsUntil x = takeWhile (<x) evenFibs

sumEvenFibsUntil :: Integer -> Integer
sumEvenFibsUntil x = sum $ evenFibsUntil x
