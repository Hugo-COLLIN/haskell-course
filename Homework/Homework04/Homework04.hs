-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

getFour :: Int
getFour = let [(_, [_, four]), _] = nested
          in four

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
short3 :: [a] -> [a]
--short3 ([a,b,c,_]) = [a,b,c]

short3 xs = case xs of
    (_:_:_:_) -> take 3 xs
    _ -> xs

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
addTuple :: (Integer, Integer, Integer) -> Integer
addTuple (a,b,c) = a + b + c


-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
isEmptyList [] = True
isEmptyList xs = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
tail2 [] = []
tail2 (x:xs) = xs


-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing.
-- (Use the `even` function to check if the number is even.)
toNextOdd :: Int -> Int
toNextOdd a = case even a of
    True -> a + 1
    False -> a

--toNextOdd a = if even a then a + 1 else a
