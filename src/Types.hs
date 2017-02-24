module Types (Position, Direction(North, East, South, West), width, height, StringMode) where

type Position = (Int, Int)
data Direction = North | East | South | West deriving Show
type StringMode = Bool

width :: Int
width = 80

height :: Int
height = 25
