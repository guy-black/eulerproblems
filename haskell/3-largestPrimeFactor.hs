

-- boooooooooo baaaad
findFactorsOld :: Int -> [Int]
findFactorsOld x =
    if (x `mod` 2 == 0) then
        filter (\y -> x `mod` y == 0) [1..(x `div` 2)]
    else if (x `mod` 3 == 0) then
        filter (\y -> x `mod` y == 0) [1..(x `div` 3)]
    else [420,69,420,69] -- this is how i'll know if i should optimize more


-- gave wrong answer, says 3 is largest prime factor of 45, should be 5.
lpf :: Int -> Int
lpf x = lpf' (((smallestFactor x),(x `div` (smallestFactor x))):[]) x
-- factorpair -> number to find factors of -> largest prime factor
lpf' :: [(Int,Int)] -> Int -> Int
lpf' ((ff, sf):acc) x =
    if (isPrime sf) then
        sf
    else
        case (dropWhile (\f -> x `mod` f /= 0) [(ff+1)..(sf-1)]) of
            [] -> case (dropWhile (isNotPrime) (ff:(map fst acc))) of
                [] -> (-42069) -- means there are no prime factors, shouldn't happen
                (f:_) -> f
            (f:_) -> -- f is the next factor to test against
                lpf' ((f, (x `div` f)):(ff,sf):acc) x










-- number we're looking for factors of, list we're looking in, accumulator
fff :: Int  -> [Int] -> [(Int, Int)] -> [(Int, Int)]
-- list should never be empty
fff _ [] acc = []
fff x (n:ns) (a:acc) =
    if (snd a) == n then -- if this factor was found already
        (a:acc)
    else if (x `mod` n) == 0 then -- if n is a factor of x
        fff x ns ((n,(x`div`n)):a:acc)
    else
        fff x ns acc

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
