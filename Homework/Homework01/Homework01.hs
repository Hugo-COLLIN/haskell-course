
-- Question 1
-- Write a multiline comment below.
{-
    Hello!
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
mulT x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
areaCircle r = pi * r * r

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder.
volumeCylinder r h = areaCircle r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
greaterEq42 r h = volumeCylinder r h > 42

