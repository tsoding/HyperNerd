module Data.Bool where

intAsBool :: Int -> Bool
intAsBool = (0 /=)

boolAsInt :: Bool -> Int
boolAsInt = fromEnum
