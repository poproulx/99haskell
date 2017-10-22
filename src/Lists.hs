module Lists
    ( myLast,
      myButLast
    ) where

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast(_:xs) = myLast xs

myButLast :: [a] -> a
--myButLast [] = error "empty list"
--myButLast [x] = error "one element list"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
