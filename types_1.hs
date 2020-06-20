import Data.Char

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then (digitToInt x) * 10 + (digitToInt y) else 100

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0  = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' :: Integer -> Integer
fibonacci' n | n == 0 = 0
             | n == 1 = 1
             | n > 0 = (helper 0 1 n) + (helper 0 1 (n - 1))
             | otherwise = (fibonacci' (n + 2)) - (fibonacci' (n + 1))

helper :: Integer -> Integer -> Integer -> Integer
helper prev _ 0 = prev
helper prv cur n = helper cur (prv + cur) (n - 1)

fibonacci'' :: Integer -> Integer
fibonacci'' n = (helper' 0 1 n)

helper' :: Integer -> Integer -> Integer -> Integer
helper' prev _ 0 = prev
helper' prv cur n | n > 0 = helper' cur (prv + cur) (n - 1)
                  | n < 0 = helper' (cur - prv) prv (n + 1)

seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 0 = helper'' 3 2 1 (n - 2)

helper'' :: Integer -> Integer -> Integer -> Integer -> Integer
helper'' curr _ _ 0 = curr
helper'' prev1 prev2 prev3 n = helper'' (prev1 + prev2 - 2 * prev3) prev1 prev2 (n - 1)