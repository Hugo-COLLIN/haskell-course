{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

-- Core Game
data Move = GoLeft | GoRight | GoForward
    deriving (Show, Eq)

data Forest staminaLoss = Path staminaLoss (Forest staminaLoss) (Forest staminaLoss) (Forest staminaLoss) | Exit
    deriving (Show)

crossForest :: (Num stamina, Ord stamina) => (stamina, Forest stamina) -> [Move] -> (stamina, Forest stamina)
crossForest (stamina, Exit) _ = (stamina, Exit)
crossForest (stamina, forest) [] = (stamina, forest)
crossForest (stamina, Path cost left right forward) (mv:mvl)
    | stamina <= 0 = (stamina, Path cost left right forward)
    | otherwise = case mv of
        GoLeft -> crossForest (stamina - cost, left) mvl
        GoRight -> crossForest (stamina - cost, right) mvl
        GoForward -> crossForest (stamina - cost, forward) mvl

showCurrentChoice :: (Num stamina, Show stamina, Ord stamina) => (stamina, Forest stamina) -> String
showCurrentChoice (stamina, Exit)
    | stamina <= 0 = "Finding the exit, you ran out of stamina and died -.-!"
    | otherwise = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice (stamina, Path {})
    | stamina <= 0 = "You ran out of stamina and died -.-!"
    | otherwise = "You have " ++ show stamina ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

play :: (Num stamina, Ord stamina, Show stamina) => Forest stamina -> [Move] -> String
play forest moves = showCurrentChoice $ crossForest (10, forest) moves

-- Test example
testForest :: Forest Int
testForest =
    Path 2  -- The initial move costs 2 stamina
        (Path 3 Exit (Path 6 Exit Exit Exit) Exit)  -- Left path (cost 3) leads to Exit
        (Path 1 (Path 8 Exit (Path 5 Exit Exit Exit) Exit) Exit (Path 4 Exit Exit Exit))  -- Right path (cost 1)
        (Path 2 (Path 2 Exit Exit Exit) (Path 3 Exit Exit Exit) Exit)  -- Forward path (cost 2)

examplePath = play testForest [GoForward, GoLeft]
exampleDied = play testForest [GoRight, GoLeft, GoRight]
exampleExitDied = play testForest [GoLeft, GoRight, GoRight]
exampleExit = play testForest [GoLeft, GoLeft]


