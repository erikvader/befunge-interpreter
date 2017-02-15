module BProgramCounter (BProgramCounter, Direction, starting, reverse, step, getPosition) where

import Prelude hiding (reverse)

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

starting :: BProgramCounter
reverse :: BProgramCounter -> BProgramCounter
step :: BProgramCounter -> BProgramCounter
getPosition :: BProgramCounter -> Position
setDirection :: BProgramCounter -> Direction -> BProgramCounter

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

type Position = (Int, Int)
data Direction = North | East | South | West deriving (Show)
newtype BProgramCounter = PC (Position, Direction) deriving (Show)


starting = PC ((0, 0), East)


reverse (PC (pos, North)) = PC (pos, South)
reverse (PC (pos, East)) = PC (pos, West)
reverse (PC (pos, South)) = PC (pos, North)
reverse (PC (pos, West)) = PC (pos, East)


step (PC ((x, y), North)) = PC ((x, y - 1), North)
step (PC ((x, y), East)) = PC ((x + 1, y), East)
step (PC ((x, y), South)) = PC ((x, y + 1), South)
step (PC ((x, y), West)) = PC ((x - 1, y), West)


getPosition (PC (pos, _)) = pos


setDirection (PC (pos, _)) newdir = PC (pos, newdir)