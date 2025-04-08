{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Forest.Level1 (Forest(..), level1forest) --helperFunction won't work because not exposed
import User.Actions.Move (AvailableMoves(..), move)
import System.Random (randomRIO)

main :: IO ()
main = do
        startingStamina <- randomRIO @Int (10_000,20_000) -- @Int allowed by TypeApplications
        putStrLn "You're traped in a Forest, try to scape! Remember that you loose stamina with each step you take"
        gameLoop (startingStamina, level1forest)
    where
        gameLoop (_, Exit) = putStrLn "CONGRATULATIONS!! YOU'VE FOUND THE EXIT!!"
        gameLoop (s, _) | s <= 0 = putStrLn "You ran out of stamina and died -.-!"
        gameLoop (s, forest) = do
            putStrLn $ "You have " ++ show s ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, GoForward!"
            selectedMove <- getLine
            gameLoop $ move (s, forest) (read @AvailableMoves selectedMove)
