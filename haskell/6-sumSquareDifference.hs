sumSqrs :: [Int] -> Int
sumSqrs x = sum (map (\y -> y*y) x)

sqrSums :: [Int] -> Int
sqrSums x = (\y -> y*y) (sum x)


difference :: Int -> Int -> Int
difference a b
    | a > b = a - b
    | otherwise = b - a

answer = difference (sumSqrs [1..1000]) (sqrSums [1..1000])


-- this one is really quick, even with a list as large as [1..9999999]
-- answer is 7113094025547934784 btw
-- [1..9999999999] is too big tho
