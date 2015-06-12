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

factorial :: Integer -> Integer
factorial n = product [1..n]

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
