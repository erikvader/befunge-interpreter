module BInstructionPointer (BInstructionPointer, Direction, starting, reverse, step, getPosition) where

import Prelude hiding (reverse)

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

starting :: BInstructionPointer
reverse :: BInstructionPointer -> BInstructionPointer
step :: BInstructionPointer -> BInstructionPointer
getPosition :: BInstructionPointer -> Position
setDirection :: BInstructionPointer -> Direction -> BInstructionPointer

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

type Position = (Int, Int)
data Direction = North | East | South | West deriving (Show)
newtype BInstructionPointer = IP (Position, Direction) deriving (Show)


starting = IP ((0, 0), East)


reverse (IP (pos, North)) = IP (pos, South)
reverse (IP (pos, East)) = IP (pos, West)
reverse (IP (pos, South)) = IP (pos, North)
reverse (IP (pos, West)) = IP (pos, East)


step (IP ((x, y), North)) = IP ((x, y - 1), North)
step (IP ((x, y), East)) = IP ((x + 1, y), East)
step (IP ((x, y), South)) = IP ((x, y + 1), South)
step (IP ((x, y), West)) = IP ((x - 1, y), West)


getPosition (IP (pos, _)) = pos


setDirection (IP (pos, _)) newdir = IP (pos, newdir)