import Data.Char
import Data.Function -- for `on`
import Data.List -- for unfoldr

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

toDouble :: Int -> Double
toDouble = fromInteger . toInteger

avg :: Int -> Int -> Int -> Double
avg a b c = (toDouble a + toDouble b + toDouble c) / 3

sum', product' :: (Num a) => [a] -> a

sum' [x] = x
sum' (x : xs) = x + sum xs

product' [x] = x
product' (x : xs) = x * product' xs

maximum', minimum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x : xs) | x >= maximum' xs = x
                  | otherwise        = maximum' xs

minimum' [x] = x
minimum' (x : xs) | x <= minimum' xs = x
                  | otherwise        = minimum' xs

reverse' :: [a] -> [a]
reverse' l = rev l [] where
    rev [] a       = a
    rev (x : xs) a = rev xs (x : a)

isPalindrome l | l == reverse' l = True
               | otherwise       = False

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' []       = True
isPalindrome' [x]      = True
isPalindrome' (x : xs) = (x == last xs) && (isPalindrome' $ init xs)

head' [] = 0
head' (x : xs) = x

tail' [] = []
tail' (x : xs) = xs

sum3 :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a b c = ((head' a) + (head' b) + (head' c)) : sum3 (tail' a) (tail' b) (tail' c)

rev' a = rev'' a [] where
    rev'' [] res = res
    rev'' (a : as) res = rev'' as (a : res)

groupElems :: Eq a => [a] -> [[a]]
groupElems a = rev' (groupHelper a [] []) where
    groupHelper [] [] res = res
    groupHelper [] lst res = lst : res
    groupHelper (a : as) [] res = groupHelper as [a] res
    groupHelper (a : as) lst res | a == (head lst) = groupHelper as (a : lst) res
                                 | otherwise       = groupHelper as [a] (lst : res)

filterFunction x | x == '0'  = True
                 | x == '1'  = True
                 | x == '2'  = True
                 | x == '3'  = True
                 | x == '4'  = True
                 | x == '5'  = True
                 | x == '6'  = True
                 | x == '7'  = True
                 | x == '8'  = True
                 | x == '9'  = True
                 | otherwise = False

readDigits :: String -> (String, String)
readDigits = span filterFunction -- for "365ads" result equals to ("365", "ads")

predicatesDisj p1 p2 x = (p1 x) || (p2 x)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2  = filter $ predicatesDisj p1 p2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort a  = (qsort (filter (< (head a)) a)) ++ (filter (== (head a)) a) ++ (qsort (filter (> (head a)) a))

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x * x, x * x * x])

perms' :: Eq a => [a] -> [[a]]
perms' [] = [[]]
perms' a = concatMap (\x -> map ((:) x) (perms' (filter (not . (== x)) a))) a

allShifts (a : as) n res | n == 0    = res
                         | otherwise = allShifts (as ++ [a]) (n - 1) ((a : as) : res)

perms :: [a] -> [[a]]
perms [] = [[]]
perms a = concatMap (\x -> map ((:) (head x)) (perms (tail x))) (allShifts a (length a) [])

delAllUpper :: String -> String
delAllUpper = unwords . filter (any (not . isUpper)) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> max (max a b) c)

fibStream :: [Integer]
fibStream = zipWith (+) (0 : fibStream) (0 : 1 : fibStream)

fibStream' :: [Integer]
fibStream' = 0 : 1 : [a + b | (a, b) <- zip fibStream' (tail fibStream')]

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    succ (Odd a) = Odd (a + 2)
    pred (Odd a) = Odd (a - 2)
    toEnum a = (Odd (toInteger (a)))
    fromEnum (Odd a) = fromIntegral a
    enumFrom = iterate succ
    enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
    enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
    enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]

data Odd' = Odd' Integer 
  deriving (Eq, Show)

instance Enum Odd' where
  succ (Odd' n)                             = Odd' . succ . succ $ n
  pred (Odd' n)                             = Odd' . pred . pred $ n
  toEnum n                                  = Odd' (toInteger n)
  fromEnum (Odd' n)                         = fromInteger n
  enumFrom (Odd' n)                         = map Odd' (enumFromThen n (succ . succ  $ n))
  enumFromThen (Odd' x) (Odd' y)            = map Odd' (enumFromThen x y)
  enumFromTo (Odd' x) (Odd' y)              = map Odd' (enumFromThenTo x (succ. succ $ x) y)
  enumFromThenTo (Odd' x) (Odd' y) (Odd' z) = map Odd' (enumFromThenTo x y z)

change :: (Ord a, Num a) => a -> [[a]]
change n | n == 0 = [[]]
         | otherwise = [x : exchanged | x <- [2, 3, 7], x <= n, exchanged <- change $ n - x]

concatList :: [[a]] -> [a]
concatList = foldr (++) []

-- get a = 1

-- lengthList :: [a] -> Int
-- lengthList = foldr ((+) . get)

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> (s + x * (x `mod` 2))) 0

sumOdd' :: [Integer] -> Integer
sumOdd' = foldr (\x s -> if odd x then x + s else s) 0

meanList :: [Double] -> Double
meanList = (\(f, s) -> f / s) . foldr (\x (s, k) -> (s + x, k + 1)) (0, 0)

evenOnly' :: [a] -> [a]
evenOnly' l = ((\(r, pos) -> r) . foldl (\(r, pos) x -> if pos `mod` 2 == 0 then (r ++ [x], pos + 1) else (r, pos + 1)) ([], 1)) l

evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\a (xs, ys) -> (a : ys, xs)) ([], [])

evenOnly'' :: [a] -> [a]
evenOnly'' = (foldr (\(n, x) xs -> if even n then x:xs else xs) []) . (zip [1..])

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(f, s) -> if f > s then Nothing else Just (s, (f, pred s)))
