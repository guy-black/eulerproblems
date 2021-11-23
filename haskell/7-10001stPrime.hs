answer = head (drop 10000 primes)

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
