-- find the sum of all the multiples of 3 or 5 below 1000

-- pass in 1000 or whatever
sumMult :: Integer -> Integer
sumMult n = sum (filter (\x -> ((x `mod` 3) == 0)||((x `mod` 5) == 0)) [1..(n-1)])
