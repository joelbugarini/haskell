doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

boomBangs xs = [ if x < 10 then show x else "BANG!" | x <- xs, odd x] 

lenght' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

