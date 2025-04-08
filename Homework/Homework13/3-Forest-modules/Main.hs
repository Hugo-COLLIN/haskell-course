module Main where

import Forest.Level1 (Forest(..), level1forest)
import User.Actions.Move (AvailableMoves(..), move)

main :: IO ()
main = do
        putStrLn "You're traped in a Forest, try to scape! Remember that you loose stamina with each step you take"
        gameLoop (10, level1forest)
    where
        gameLoop (_, Exit) = putStrLn "CONGRATULATIONS!! YOU'VE FOUND THE EXIT!!"
        gameLoop (s, _) | s <= 0 = putStrLn "You ran out of stamina and died -.-!"
        gameLoop (s, forest) = do
            putStrLn $ "You have " ++ show s ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, GoForward!"
            selectedMove <- getLine
            gameLoop $ move (s, forest) (read selectedMove :: AvailableMoves)
