Main page of notes:
https://github.com/MrHakimov/Guides/wiki/Haskell

Link to the online-book: http://learnyouahaskell.com/chapters

Link to the online-book: http://learnyouahaskell.com/chapters

### GHCI
`:l fileName (without .hs)` - to start working with specified file

`:r` - compile?

`:q` - leave GHCI

`:set +s` - to track space and time used

`ghc --make fileName` - compiles the program

`./fileName` - runs the program

`-- comment` - one line comment in Haskell

`{- blablabla -}` - multiline comment in Haskell

### Data types
`Int` - can store integer numbers from -2^63 to 2^63

```hs
maxInt = maxBound :: Int
minInt = minBound :: Int
```

`Integer` - unbounded (can be as big as memory can hold)

`Float` - single precision floating point numbers

`Double` - has decimals inside of it (has precision up to 11 points)

```hs
bigFloat = 3.999999999999 + 0.000000000005 -- 4.0000000000039995
bigFloat = 3.99999999999 + 0.00000000005 -- 4.00000000004
```

`Bool` - True or False
`Char` - a single characters (')
An array of `Char`'s in Haskell is equivalent to type `String`.
`Tuple` - list made up of different data types.

```hs
always5 :: Int
always5 = 5 -- it's never going to change, because there is no variables in Haskell and everything is immutable
```

Another stuff:
```hs
sumOfNums = sum [1..1000] -- 500500

addEx = 5 + 4 -- 9
subEx = 5 - 4 -- 1
multEx = 5 * 4 -- 20
divEx = 5 / 4 -- 1.25

modEx = mod 5 4 -- mod is a prefix operator
modEx2 = 5 `mod` 4 -- but we can do it infix

negNumEx = 5 + (-4) -- it's necessar to put brackets if it's an unary operator

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9) -- 3.0
```
`fromIntegral :: (Integral a, Num b) => a -> b` - takes Int (which is an instance of Integral) and "makes" it a Num.

If you type `:t sqrt` in GHCI, you'll get something like this:
```hs
sqrt :: Floating a => a -> a
```
The name of this is **type declaration**.

So, `sqrt` expects a `Floating` type, and `Floating` inherit from `Fractional`, which inherits from `Num`, so you can safely pass to `sqrt` the result of `fromIntegral`

### Built-in math functions
`pi` - value of pi

`exp` - exponent

`log` - natural logarithm

`**` - power

`truncate, round, ceiling, floor` - respective functions

```hs
sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, asinh, acosh, atanh
```

```hs
roundVal = round 7.5 -- 8

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)
```

### Lists
```hs
primeNumbers = [3, 5, 7, 11]

morePrimes = primeNumbers ++ [13, 17, 19, 23, 29] -- ++ - joining two lists

favNums = 2 : 7 : 21 : 66 : [] -- kind of append

multList = [[3, 5, 7], [11, 13, 17]]

morePrimes2 = 2 : morePrimes

lenPrime = length morePrimes2 -- 10

revPrime = reverse morePrimes2

isListEmpty = null morePrimes2

secondPrime = morePrimes2 !! 1 -- taking by index

firstPrime = head morePrimes2

lastPrime = last morePrimes2

primeInit = init morePrimes2 -- all elements except last

first3Primes = take 3 morePrimes2 -- first n elements

removedPrimes = drop 3 morePrimes2 -- last n elements

is7InList = 7 `elem` morePrimes2

maxPrime = maximum morePrimes2

minPrime = minimum morePrimes2

newList = [2, 3, 5]

prodPrimes2 = product newList

zeroToTen = [0..10]

evenList = [2, 4..20] -- the difference of 2 first values defines reminder in arithmetic progression

revEvenList = [20, 18..2]

letterList = ['A', 'C'..'Z']

infinPow10 = [10, 20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1, 2, 3, 4, 5])

listTimes2 = [x * 2 | x <- [1..10], x * 3 <= 18]

divisBy9N13 = [x | x <- [1..500], mod x 13 == 0, mod x 9 == 0]

sortedList = sort [3, 1, 2] -- requires import Data.List

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10] -- [7,9,11,13,15]
sumOfLists1 = zipWith (+) [1,2,3,4] [6,7,8,9,10] -- [7,9,11,13]
sumOfLists2 = zipWith (+) [1,2,3,4,5] [6,7,8,9] -- [7,9,11,13]

listBiggetThen5 = filter (< 12) $ filter (> 5) morePrimes2

evensUpTo20 = takeWhile (<= 20) [2,4..]

multOfList = foldl (*) 1 [2,3,4,5] -- левая свертка, третий аргумент - начальное значение
-- foldr - правая свертка

pow3List = [3^n | n <- [1..10]]
pow3List1 = [3**n | n <- [1..10]]

{-
typically for integers
(^) :: (Num a, Integral b) => a -> b -> a

typically for rationals
(^^) :: (Fractional a, Integral b) => a -> b -> a

typically for floating-point numbers
(**) :: Floating a => a -> a -> a
-}

multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
```

### Tuples
```hs
randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith

bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "345 South"]

namesNAddresses = zip names addresses -- zips into tuples
```

### Functions

Using `let` in GHC we can define functions. Another thing that we can do is to create `main` function and we will be able to compile and run our program.

```hs
main = do
	putStrLn "What's your name?"
	name <- getLine
	putStrLn ("Hello, " ++ name)
```

The name of the functions can't start with an uppercase letter.

```hs
-- funcName param1 param1 = operations (returned value)

addMe :: Int -> Int -> Int
addMe x y = x + y

sumMe x y = x + y -- we didn't declared the type => we can add floats also, for example

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _ = "Nothing important" -- it's better to put _ if you don't use the "variable" name

factorial 0 = 1
factorial n = n * factorial (n - 1)

prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
	| mod n 2 == 0 = False
	| otherwise = True

isEven n = mod n 2 == 0

whatGrade :: Int -> String
whatGrade age
	| (age >= 5) && (age <= 6) = "Kindergarten"
	| (age > 6) && (age <= 10) = "Elementary School"
	| (age > 10) && (age <= 14) = "Middle School"
	| (age > 14) && (age <= 18) = "High School"
	| otherwise = "Go to college"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg1 <= 0.2 = "Terrible Batting Average"
	| avg2 <= 0.25 = "Average Player"
	| avg1 <= 0.28 = "You're doing pretty good"
	| otherwise = "You're a Superstar"
	where
		avg1 = hits / atBats
		avg2 = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x : []) = "Your list starts with " ++ show x
getListItems (x : y : []) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x : xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x : xs) = "The first letter in " ++ all ++ " is " ++ [x] -- `all` can be replaced with any other name, [x] - shows without quotes

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1, 2, 3]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x : xs) = times4 x : multBy4 xs

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x : xs) (y : ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int) -- we can not write this line also, and it would work
getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4


threePlusList = map adds3 [1,2,3,4,5]
```

### Lambdas
Lamdas are just a way to create a functions that don't have a name.
```hs
dblTo10 = map (\x -> x * 2) [1..10] -- \x - is what would be received
```

### Conditionals
```hs
< > <= >= == /=
/= - inequality

&& || not
```

```hs
doubleEvenNumber y =
	if (mod y 2 /= 0)
		then y
		else y * 2 -- else should be everytime if you are using if

getClass :: Int -> String
getClass n = case n of
	5 -> "Go to Kindergarten"
	6 -> "Go to Elementary School"
	_ -> "Go away"
```

If-else-if
```hs
test y =
	if (mod y 2 /= 0)
		then y
		else
			if (mod y 4 == 0)
				then y * 4
				else y * 2
```

### Modules
Example of creating module (**MyModule.hs**):
```hs
module MyModule (doubleNumber, tripleNumber) where
	doubleNumber x = x * 2
	tripleNumber x = x * 3
```

In the file where you want to use this created module type:
```hs
import MyModule
```

### Enumerated types
```hs
data BaseballPlayer = Pitcher
					| Catcher
					| Infielder
					| Outfield
					deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print(barryBonds Outfield)
```

### Custom data types
```hs
data Customer = Customer String String Double
			deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "124 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors
 
shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Rock Beats Scissors"
shoot Scissors Paper = "Scissors Beat Paper"
shoot Scissors Rock = "Scissors Loses to Rock"
shoot Paper Scissors = "Paper Loses to Scissors"
shoot Rock Paper = "Rock Loses to Paper"
shoot _ _ = "Error"

-- We could define 2 versions of a type
-- First 2 floats are center coordinates and then radius for Circle
-- First 2 floats are for upper left hand corner and bottom right hand corner 
-- for the Rectangle
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
	deriving (Show)
 
-- :t Circle = Float -> Float -> Float -> Shape
 
-- Create a function to calculate area of shapes
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 -y))
 
-- Could also be area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 -y)
-- $ means that anything that comes after it will take precedence over anything 
-- that comes before (Alternative to adding parentheses)
 
-- The . operator allows you to chain functions to pass output on the right to
-- the input on the left
-- sumValue = putStrLn (show (1 + 2)) becomes
sumValue = putStrLn . show $ 1 + 2
 
-- Get area of shapes
areaOfCircle = area (Circle 50 60 20)
areaOfRectangle = area $ Rectangle 10 10 100 100
```

### Type classes
```hs
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor
```

vs

```hs
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) -- it's called `Record syntax`
```

Notice that when defining a `Person`, we used the same name for the data type and the value constructor. This has no special meaning, although it's common to use the same name as the type if there's only one value constructor.

```hs
data Employee = Employee { name :: String,	
						   position :: String,
						   idNum :: Int 
						   } deriving (Eq, Show)
						   
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001}

-- We can check equality because of Eq
isSamPam = samSmith == pamMarx
 
-- We can print out data because of Show
samSmithData = show samSmith
```

Overriding show and equality methods:
```hs
-- Make a type instance of the typeclass Eq and Show
data ShirtSize = S | M | L
 
instance Eq ShirtSize where
	S == S = True
	M == M = True
	L == L = True
	_ == _ = False
 
instance Show ShirtSize where
	show S = "Small"
	show M = "Medium"
	show L = "Large"
	
-- Check if S is in the list
smallAvail = S `elem` [S, M, L]
 
-- Get string value for ShirtSize
theSize = show S
```

Custom typeclass with custom equality:
```hs
-- Define a custom typeclass that checks for equality
-- a represents any type that implements the function areEqual
class MyEq a where
	areEqual :: a -> a -> Bool
	
-- Allow Bools to check for equality using areEqual
instance MyEq ShirtSize where
	areEqual S S = True
	areEqual M M = True
	areEqual L L = True
	areEqual _ _ = False
 
newSize = areEqual M M
```

### I/O
```hs
sayHello = do
	-- Prints the string with a new line, putStr - without a new line
	putStrLn "What's your name: "
	
	-- Gets user input and stores it in name
	name <- getLine
	
	putStrLn $ "Hello " ++ name

-- requires `import System.IO`	
-- File IO
-- Write to a file 
writeToFile = do
 
	-- Open the file using WriteMode
	theFile <- openFile "test.txt" WriteMode
	
	-- Put the text in the file
	hPutStrLn theFile ("Random line of text")
	hPutStrLn theFile ("Another random line of text")
	
	-- Close the file
	hClose theFile
	
readFromFile = do
 
	-- Open the file using ReadMode
	theFile2 <- openFile "test.txt" ReadMode
	
	-- Get the contents of the file
	contents <- hGetContents theFile2
	putStr contents
	
	-- Close the file
	hClose theFile2
```

### Example -- Fibonacci numbers
```hs
-- | for every (a, b) add them
-- <- stores a 2 value tuple in a and b
-- tail : get all list items minus the first
-- zip creates pairs using the contents from 2 lists being the lists fib and the 
-- list (tail fib)
 
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib) ]
 
-- First time through fib = 1 and (tail fib) = 1
-- The list is now [1, 1, 2] because a: 1 + b: 1 = 2
 
-- The second time through fib = 1 and (tail fib) = 2
-- The list is now [1, 1, 2, 3] because a: 1 + b: 2 = 3
 
fib300 = fib !! 300 -- Gets the value stored in index 300 of the list
 
-- take 20 fib returns the first 20 Fibonacci numbers
```

### Maps
```hs
import Data.Map as Map

dict =
	[("Python", "Guido van Rossum"),
	("C", "Dennis Ritchie"),
	("Haskell", "Haskell Curry")
	]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v) : xs) = if key == k
	then Just v
	else findKey key xs

-- requires `import Data.Map`
-- Map.empty - empty map
-- Map.null mapa - checks `mapa` to emptiness
-- Map.size - size of the map
-- Map.insert key value mapa
-- Map.fromList - creates a map from a list

myMap = Map.fromList dict
-- Map.insert "php" "Rasmus Lerdorf" myMap
```

More information [here](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html).

### For lab
[Functional Parsing](https://www.youtube.com/watch?v=dDtZLm7HIJs)

[JSON parser in Haskell](https://www.youtube.com/watch?v=N9RUqGYuGfw)

[Playlist with parsing videos in Haskell](https://www.youtube.com/watch?v=9AllRc64pVE&list=PL_xuff3BkASMOzBr0hKVKLuSnU4UIinKx)

[Interpreter in Haskell](https://habr.com/ru/post/335366/)

[Happy Guide](https://www.haskell.org/happy/doc/happy.pdf)

[Alex Guide](https://www.haskell.org/alex/doc/alex.pdf)

[Happy plus Alex](https://github.com/dagit/happy-plus-alex/tree/master/src)

[Alex and Happy](https://leanpub.com/alexandhappy/read)
