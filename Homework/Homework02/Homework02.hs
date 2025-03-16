
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

--f1 :: Double a => a -> a -> a -> a --false
f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?


-- Question 3
-- Why should you define type signatures for variables? How can they help you?


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
