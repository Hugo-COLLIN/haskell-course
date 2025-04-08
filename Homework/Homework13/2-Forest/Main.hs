data Forest a = Exit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

level1forest :: (Ord a, Num a) => Forest a
level1forest =
    Trail
        3
        ( Trail
            7
            (Trail 3 Exit Exit Exit)
            (Trail 4 Exit Exit Exit)
            (Trail 5 Exit Exit Exit)
        )
        ( Trail
            3
            (Trail 3 Exit Exit Exit)
            (Trail 9 Exit Exit Exit)
            (Trail 5 Exit Exit Exit)
        )
        ( Trail
            5
            (Trail 3 Exit Exit Exit)
            (Trail 4 Exit Exit Exit)
            (Trail 1 Exit Exit Exit)
        )

data AvailableMoves = GoForward | GoLeft | GoRight deriving (Show, Read)

move :: Num a => (a, Forest a) -> AvailableMoves -> (a, Forest a)
move (s, Exit) _ = (s, Exit)
move (s, Trail a x _ _) GoLeft = (s - a, x)
move (s, Trail a _ x _) GoForward = (s - a, x)
move (s, Trail a _ _ x) GoRight = (s - a, x)

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
