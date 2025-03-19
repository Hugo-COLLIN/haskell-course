-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

flipParams :: (a -> b -> c) -> b -> a -> c
flipParams func a b = func b a


-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

uncurry f a b = f (a,b)


-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).

curry f (a,b) = f a b


-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there.
hasUppercase :: String -> Bool
hasUppercase = any (\c -> c >= 'A' && c <= 'Z')


-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: String -> [String] -> Int
count team [] = 0
count team (x:xs) =
    if x == team
    then 1 + count team xs
    else count team xs

count' :: String -> [String] -> Int
count' team = length . filter (== team)

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

filterByBrand :: String -> [(String,Int)] -> Bool
--filterByBrand brand listCars = filter (\(bi, _) -> bi == brand) listCars >>= \(_,n) -> n > 0
filterByBrand brand = any (\(_,n) -> n > 0) . filter (\(bi, _) -> bi == brand)

