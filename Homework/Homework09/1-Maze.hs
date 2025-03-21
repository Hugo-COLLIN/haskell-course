{-

**************************** IMPORTANT ****************************

This week is a two-step homework. First, you have to solve the
"Maze" challenge, and then the "Forest" challenge. The challenges
are in two separate files in both the homework and solution, so
you can check the solution for the first "Maze" challenge without
spoilers of the "Forest" one. Make sure to check the solution for
"Maze" (and only "Maze," I see you 🥸👀) before starting with the
"Forest" challenge!

*******************************************************************

Today, you'll build the simplest and most basic game imaginable.
It'll be a maze game where the player has to write a list of moves, and the game will perform them
and tell the player where it ends up. Then, the player can change the moves and check again until it
finds the exit.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.

It should look like this:

*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"

How are you going to achieve this? You can try it on your own, but here you have a
step-by-step just in case:
-}

--1. Write two data types. One for the moves (Move) you can make, and another for the maze (Maze).
--(Use the example above to figure them out.)
data Move = GoLeft | GoRight | GoForward
    deriving (Show, Eq)

data Maze = Path Maze Maze Maze | Wall | Exit
    deriving (Show)

--2. Write a function called "move" that takes a maze and a move and returns the maze after the move.
move :: Maze -> Move -> Maze
move (Path left right forward) mv = case mv of
    GoLeft -> left
    GoRight -> right
    GoForward -> forward
move Wall _ = Wall
move Exit _ = Exit

--3. Write a "testMaze" value of type "Maze" and test the "move" function in GHCi.
testMaze :: Maze
testMaze = Path
    Wall
    (Path Wall Exit Wall)
    (Path (Path Exit Wall Wall) Wall Wall)

moveExample = move testMaze GoForward

--4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
--after making those moves.

solveMaze :: Maze -> [Move] -> Maze
solveMaze maze [] = maze
solveMaze (Path left right forward) (mv:mvl) = case mv of
    GoLeft -> solveMaze left mvl
    GoRight -> solveMaze right mvl
    GoForward -> solveMaze forward mvl
solveMaze Wall _ = Wall
solveMaze Exit _ = Exit

solveExample = solveMaze testMaze [GoForward, GoLeft, GoRight]


--5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
--it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
--that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
--still need to make another choice.
showCurrentChoice :: Maze -> String
showCurrentChoice (Path {}) = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
showCurrentChoice Wall = "You've hit a wall!"
showCurrentChoice Exit = "YOU'VE FOUND THE EXIT!!"

--6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D

solveMaze' :: Maze -> [Move] -> String
solveMaze' maze [] = showCurrentChoice maze
solveMaze' (Path left right forward) (mv:mvl) = case mv of
    GoLeft -> solveMaze' left mvl
    GoRight -> solveMaze' right mvl
    GoForward -> solveMaze' forward mvl
solveMaze' Wall _ = showCurrentChoice Wall
solveMaze' Exit _ = showCurrentChoice Exit

solveExample' = solveMaze' testMaze [GoForward, GoLeft]
