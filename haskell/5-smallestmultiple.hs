
-- only works for range up to 1-18
-- the smallest multiple there is 12252240 btw
-- not that anyone asked
-- after that it's too show
-- start of range -> end of range -> smallest multiple of nums in range
smallestMultipleOfRange :: Int -> Int -> Int
smallestMultipleOfRange f l =
    case (filter (check [f..l]) [1..]) of
        [] -> (-42069) -- error, no common multiple
        (a:_) -> a

check :: [Int] -> Int -> Bool
check xs n =
    and (map (\x -> n `mod` x == 0) xs)

-- copied from problem 3 for helper functions
-- these are fine i guess
isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = findFactors x == [1]
isNotPrime x = not (isPrime x)
primes :: [Int]
primes = filter isPrime [2..]
smallestFactor :: Int -> Int
smallestFactor x = head (filter (\p -> x `mod` p == 0) primes)
-- works but slow
findFactors :: Int -> [Int]
findFactors x =
    filter (\y -> x `mod` y == 0) [1..(x `div` 2)]

