module BProgramCounter (starting, reverse, step, getPosition, setPosition, setDirection, getDirection, isStringMode, setStringMode) where

import Prelude hiding (reverse)
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

starting      :: BProgramCounter
reverse       :: BProgramCounter -> BProgramCounter
step          :: BProgramCounter -> BProgramCounter
getPosition   :: BProgramCounter -> Position
setPosition   :: BProgramCounter -> Position -> BProgramCounter
setDirection  :: BProgramCounter -> Direction -> BProgramCounter
getDirection  :: BProgramCounter -> Direction
isStringMode :: BProgramCounter -> StringMode
setStringMode :: BProgramCounter -> StringMode -> BProgramCounter

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

starting = PC ((0, 0), East, False)

{-
reverse (PC (pos, North)) = PC (pos, South)
reverse (PC (pos, East)) = PC (pos, West)
reverse (PC (pos, South)) = PC (pos, North)
reverse (PC (pos, West)) = PC (pos, East)
-}

reverse pc = setDirection pc (turnaround (getDirection pc))
   where
      turnaround West = East
      turnaround East = West
      turnaround North = South
      turnaround South = North


step pc@(PC ((x, y), North, _)) = setPosition pc (x, mod (y - 1) height)
step pc@(PC ((x, y), East, _))  = setPosition pc (mod (x + 1) width, y)
step pc@(PC ((x, y), South, _)) = setPosition pc (x, mod (y + 1) height)
step pc@(PC ((x, y), West, _))  = setPosition pc (mod (x - 1) width, y)

getPosition (PC (pos, _, _)) = pos
setPosition (PC (_, dir, sm)) pos = PC (pos, dir, sm)

getDirection (PC (_, dir, _)) = dir
setDirection (PC (pos, _, sm)) newdir = PC (pos, newdir, sm)

isStringMode (PC (_, _, sm)) = sm
setStringMode (PC (pos, dir, _)) sm = PC (pos, dir, sm)
