module Lists
    ( myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse
    ) where

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast(_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Integer -> a
elementAt [] _ = error "empty list"
elementAt (x:_) 1 = x
elementAt (x:xs) y
  | y > 0 = elementAt xs (y - 1)
  | otherwise = error "out of bound index"

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
--myReverse (x:xs) = (myReverse xs) ++ [x]
myReverse xs = last xs:myReverse (init xs)