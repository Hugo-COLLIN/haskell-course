-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

--The Bounded type class provides an upper and lower limit for a type. Any type that is an instance of Bounded has two predefined values:
--    minBound: The lowest possible value of the type.
--    maxBound: The highest possible value of the type.

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

--Both Int and Word are integral types in Haskell (they are instances of common type classes - Bounded, Enum, Num, etc.), and both implement the same type classes, but they differ in how they represent numbers:
--    Int is a signed integer type (can represent both positive and negative values)
--    Word is an unsigned integer type (can only represent non-negative values)

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

--The Enum type class provides sequential ordering and allows iteration over values. It enables:
--    Successor and predecessor functions: succ and pred
--    Enumeration using list ranges: [start..end]
--    Conversion between values and their integer representations: fromEnum and toEnum

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

--f1 :: (Num a, String b) => a -> a -> b -> b
--f1 :: (Show a, Fractional a, Char b) => a -> a -> [b] -> [b]
f1 :: (Show a, Fractional a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x / y) ++ z

--f2 :: Bounded a => a -> a
f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

--Haskell provides several numeric type classes that enable conversions between different number types. Important Numeric Type Classes:
--    Num: Basic arithmetic (+, -, *)
--    Fractional: Supports division (/)
--    Integral: Only for whole numbers (Int, Integer)
--    Real: Provides toRational
--    RealFrac: Allows properFraction, floor, ceiling
--    Floating: Supports functions like sin, cos, exp
--    RealFloat: Provides functions like decodeFloat, isNaN
--
--For explicit conversions between numeric types, the most commonly used functions are:
--    fromIntegral :: (Integral a, Num b) => a -> b - Converts any Integral type to any Num type
--    realToFrac :: (Real a, Fractional b) => a -> b - Converts any Real type to any Fractional type
--    Type-specific conversion functions like toInteger, toRational
--    Rounding functions: truncate, round, ceiling, floor
