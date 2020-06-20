import Data.Char
import Data.Function -- for `on`

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

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n | n == 0 = (0, 1)
              | n > 0 = helper''' 0 0 n
              | otherwise = helper''' 0 0 (-n)

helper''' :: Integer -> Integer -> Integer -> (Integer, Integer)
helper''' sum cnt 0 = (sum, cnt)
helper''' sum cnt n = helper''' (sum + (mod n 10)) (cnt + 1) (div n 10)

getSecondFrom :: a -> b -> c -> b    -- Polymorphism of function arguments
getSecondFrom a b c = b              -- This function can be called on arguments of any types

multSecond = g `on` h

g a b = a * b

h p = snd p

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- doItYourself = f . g . h

-- f = logBase 2

-- g = (^ 3)

-- h = max 42

class Printable a where
	toString  :: a -> String

instance Printable Bool where
	toString True  = "true"
	toString False = "false"
	toString _     = "error"

instance Printable () where
	toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
	toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a = if (doesEnrageGork a && doesEnrageMork a)
    	then
    		(stomp (stab a))
    	else
    		if (doesEnrageMork a)
    			then (stomp a)
    			else
    				if (doesEnrageGork a)
    					then (stab a)
    					else a

a = 127.22
b = 4.12
c = 0.1
d = 2
ip = show a ++ show b ++ show c ++ show d

class (Bounded a, Eq a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
  	| a == maxBound = minBound
  	| otherwise     = succ a

  spred :: a -> a
  spred a
    | a == minBound = maxBound
    | otherwise     = pred a