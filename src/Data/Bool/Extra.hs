module Data.Bool.Extra
  ( intAsBool
  , boolAsInt
  ) where

intAsBool :: Int -> Bool
intAsBool 0 = False
intAsBool _ = True

boolAsInt :: Bool -> Int
boolAsInt True = 1
boolAsInt False = 0
