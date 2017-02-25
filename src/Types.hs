module Types (Position, Direction(North, East, South, West), width, height, StringMode) where

{- REPRESENTATION CONVENTION:
      - (x, y) represents a position on the grid in BMemory where x is the x coordinate
               and y is the y coordinate.
   REPRESENTATION INVARIANT:
      (x, y), it must be true that:
         - 0 <= x < width
         - 0 <= y < height
-}
type Position = (Int, Int)

{- REPRESENTATION CONVENTION:
      Represents a direction the BProgramCounter can face on the grid.
      - North is upwards
      - East is to the right
      - West is to the left
      - South is downwards

   REPRESENTATION INVARIANT: True

-}
data Direction = North | East | South | West deriving Show

{- REPRESENTATION CONVENTION: Whether the befunge program is in string mode or not.
   REPRESENTATION INVARIANT: True
-}
type StringMode = Bool

--the width of the grid
width :: Int
width = 80

--the height of the grid
height :: Int
height = 25
