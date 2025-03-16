-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly.
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
check maxAllowed consumption hoursPerDay =
    let monthlyUsage = consumption * hoursPerDay * 30
    in monthlyUsage < maxAllowed

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
overConsumption maxAllowed consumption hoursPerDay
    | monthlyUsage < maxAllowed = 0
    | otherwise = monthlyUsage - maxAllowed
    where
    monthlyUsage = consumption * hoursPerDay * 30

overConsumptionMsg maxAllowed consumption hoursPerDay =
    let monthlyUsage = consumption * hoursPerDay * 30
        overConsumed = if monthlyUsage < maxAllowed
            then 0
            else monthlyUsage - maxAllowed
    in "Overconsumption is about " ++ show overConsumed ++ "kWh"


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.
quotient a b = if b == 0
    then "Error: Dividing by 0"
    else show (a `mod` b)


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
sumSqrt a b =
    let result = sqrt productVal + sqrt quotientVal
    in result
    where
        productVal = a*b
        quotientVal = if b /= 0 then a/b else 0
