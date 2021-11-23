import Data.List

isPalNum :: Int -> Bool
isPalNum n = (show n) == (reverse (show n))

lpp = last (Data.List.sort (filter isPalNum [x * y | x<-[100..999], y<-[100..999]]))
