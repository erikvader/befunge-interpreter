module BProgramCounter (starting, reverse, step, getPosition, setDirection, getDirection) where

import Prelude hiding (reverse)
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

starting :: BProgramCounter
reverse :: BProgramCounter -> BProgramCounter
step :: BProgramCounter -> BProgramCounter
getPosition :: BProgramCounter -> Position
setDirection :: BProgramCounter -> Direction -> BProgramCounter
getDirection :: BProgramCounter -> Direction

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

starting = PC ((0, 0), East)


reverse (PC (pos, North)) = PC (pos, South)
reverse (PC (pos, East)) = PC (pos, West)
reverse (PC (pos, South)) = PC (pos, North)
reverse (PC (pos, West)) = PC (pos, East)


step (PC ((x, y), North)) = PC ((x, mod (y - 1) height), North)
step (PC ((x, y), East)) = PC ((mod (x + 1) width, y), East)
step (PC ((x, y), South)) = PC ((x, mod (y + 1) height), South)
step (PC ((x, y), West)) = PC ((mod (x - 1) width, y), West)


getPosition (PC (pos, _)) = pos


setDirection (PC (pos, _)) newdir = PC (pos, newdir)
getDirection (PC (_, dir)) = dir
