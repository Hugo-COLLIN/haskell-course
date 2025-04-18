-- 1 module per file
module Forest.Level1 (Forest(..), level1forest) where

data Forest a = Exit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

helperFunction = 6 -- not exposed to the outside (cf module signature)

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
