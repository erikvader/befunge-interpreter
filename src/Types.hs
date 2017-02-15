module Types (Position, Direction(North, East, South, West), BProgramCursor(PC)) where

type Position = (Int, Int)
data Direction = North | East | South | West
newtype BProgramCursor = PC (Position, Direction) --TODO add string mode
