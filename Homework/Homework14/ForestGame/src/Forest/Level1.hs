{-# LANGUAGE NumericUnderscores #-}

module Forest.Level1 (Forest(..), level1forest) where

data Forest a = Exit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

level1forest :: (Ord a, Num a) => Forest a
level1forest =
    Trail
        3_000 -- allowed by NumericUnderscores
        ( Trail
            7_000
            (Trail 3000 Exit Exit Exit)
            (Trail 4000 Exit Exit Exit)
            (Trail 5000 Exit Exit Exit)
        )
        ( Trail
            3000
            (Trail 3000 Exit Exit Exit)
            (Trail 9000 Exit Exit Exit)
            (Trail 5000 Exit Exit Exit)
        )
        ( Trail
            5000
            (Trail 3000 Exit Exit Exit)
            (Trail 4000 Exit Exit Exit)
            (Trail 10000 Exit Exit Exit)
        )
