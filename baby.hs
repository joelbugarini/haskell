doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

boomBangs xs = [ if x < 10 then show x else "BANG!" | x <- xs, odd x] 

length' xs = sum [1 | _ <- xs]

removeNonUppercase xs = [ x | x <- xs, x `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial' :: Integer -> Integer
factorial' n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

--P1 Find the last element of a list.
myLast :: [a] -> a
myLast x = last x

--P2 Find the last but one element of a list.
myButLast :: [a] -> a
myButLast x = last (init x)

--P3 Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [b] -> Int -> b
elementAt b x = last (take x b)

--P4 Find the number of elements of a list.
myLength :: [list] -> Int
myLength list = length list

--P5 Reverse a List
myReverse :: [list] -> [list]
myReverse list = reverse list



--Pattern Matching
lucky :: (Integral a) => a -> String
lucky 7 = "Jackpot"
lucky x = "Out of luck babe!"

--Factorial Pattern Matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

--List Pattern Matching
headita :: [a] -> a
headita [] = error "cant call headita on empty list, -.-"
headita (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "List empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++show y
tell (x:y:_) = "No mames : " ++ show x ++ " and " ++show y

length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length' xs 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Just Patterns
capital :: String -> String
capital "" = "Empty string puto"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--Guards
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

--Initials
--initials :: String -> String -> String
--initials firstname lastname =
--    where (f:_) = firstname
--          (l:_) = lastname

--Max with guards
max' :: (Ord a) => a -> a -> a
max' a b 
	| a > b = a 
	| otherwise = b

--Compare with Guards
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

--Very similar  WHERE = LET
--WHERE - All the function can see the construct
--LET - Is more local

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

--Case expressions
--This is PATTERN MATCHING
--Just sytactic sugar for case exp.
head' :: [a] -> a
head' [] = error "No head, it's empty"
head' (x:_) = x

--This is CASE EXPRESSIONS
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head, it's empty"
                       (x:_) -> x

--case expression of pattern -> result  
--                   pattern -> result  
--                   pattern -> result  
--                   ...  

--[let square x = x * x in (square 5, square 3, square 2)]
--(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
 
--P7 Transform a list, possibly holding lists as elements into a 
--'flat' list by replacing each list with its elements (recursively).



---------------------------------------
--           RECURSION               --
---------------------------------------

--To myself:
--It's my duty to learn and breath recursion, 
--to see and reduce complexity to a joke.


--maximum
--maximum' :: (Ord a) => [a] -> a
--maximum' [] = error "maximum of empty list"
--maximum' [x] = x
--maximum' (x:xs)
--    | x > maxTail = x
--    | otherwise = maxTail
--    where maxTail = maximum' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' a b
    | a <= 0    = []
    | otherwise = b:replicate' (a-1) b

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' a _
    | a <= 0 = []
take' _ []   = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--P6 Find out whether a list is a palindrome. 
--A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq list) => [list] -> Bool
isPalindrome a
    | a == x    = True
    | otherwise = False 
    where x = reverse a

--P7 Flatten a nested list structure.
--Transform a list, possibly holding lists as elements into a 'flat'
-- list by replacing each list with its elements (recursively).
--data NestedList a = Elem a | List [NestedList a]

--flatten :: [a] -> [a]
--flatten [] = []
--flatten (x:xs) = x : flatten xs

--P8 Eliminate consecutive duplicates of list elements.
--If a list contains repeated elements they should be replaced with a
--single copy of the element. The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)  
    | x `elem` xs = compress xs
    | otherwise   = x : compress xs

--P9 Pack consecutive duplicates of list elements into sublists. If a list 
--contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [a]
pack [] = []
pack [x] = [x]
pack (x:xs)  
    | x `elem` xs = pack xs
    | otherwise   = x : pack xs

--Mapping to alist with mean values
--meaned :: [Num a] -> [Num a]
--meaned [] = []
--meaned [x] = [x]
--meaned (x:xs) 
--	| x == -1   = meaned(xs) 
--	| otherwise = x:xs

--next :: (Num a) => [a] -> a
--next [x] = x 
--next (x:xs) 
--	| x ==-1   = next(xs) 
--	| otherwise = x

--map 
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f x:xs = f x : map f xs

--filter :: 