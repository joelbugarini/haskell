doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

boomBangs xs = [ if x < 10 then show x else "BANG!" | x <- xs, odd x] 

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

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

--P6 Find out whether a list is a palindrome. 
--A palindrome can be read forward or backward; e.g. (x a m a x).
--isPalindrome :: [list] -> Bool
--isPalindrome list = if list == reverse list 
--                      then True
--                      else False

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

--[let square x = x * x in (square 5, square 3, square 2)]
--(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
 
--P7 Transform a list, possibly holding lists as elements into a 
--'flat' list by replacing each list with its elements (recursively).

