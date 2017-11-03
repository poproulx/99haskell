module Lists
    ( myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse,
      isPalindrome,
      NestedList(List, Elem),
      flatten,
      compress
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

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem element) = [element]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress ::Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x:compress xs
