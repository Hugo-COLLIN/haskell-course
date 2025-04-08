module User.Actions.Move where

import Forest.Level1 (Forest(..))

data AvailableMoves = GoForward | GoLeft | GoRight deriving (Show, Read)

move :: Num a => (a, Forest a) -> AvailableMoves -> (a, Forest a)
move (s, Exit) _ = (s, Exit)
move (s, Trail a x _ _) GoLeft = (s - a, x)
move (s, Trail a _ x _) GoForward = (s - a, x)
move (s, Trail a _ _ x) GoRight = (s - a, x)
