module BProgramCounter (BProgramCounter, starting, reverse, step, getPosition, setPosition, setDirection, getDirection, isStringMode, setStringMode) where

import Prelude hiding (reverse)
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

--the counter when the befunge program starts
starting :: BProgramCounter

{- reverse programCounter
  PRE: True
  POST: programCounter with the direction reversed
  SIDE EFFECTS: None
  Examples:
    reverse (PC (pos, North, False)) = PC (pos, South, False)
    reverse (PC (pos, East, False)) = PC (pos, West, False)
    reverse (PC (pos, South, False)) = PC (pos, North, False)
    reverse (PC (pos, West, False)) = PC (pos, East, False)
-}
reverse :: BProgramCounter -> BProgramCounter

{- step programCounter
  PRE: True
  POST: programCounter after taking one step in the direction it is facing
  SIDE EFFECTS: None
  EXAMPLES:
    step (PC ((2, 5), East, False)) == PC ((3, 5), East, False)
    step (PC ((2, 5), North, False)) == PC ((2, 4), North, False)
-}
step :: BProgramCounter -> BProgramCounter

{- getPosition programCounter
   PRE: True
   POST: programCounter's current position as a tuple (x, y)
   SIDE EFFECTS: None
   EXAMPLES:
    getPosition PC ((2, 5), East, False) == (2, 5)
-}
getPosition :: BProgramCounter -> Position

{- setPosition programCounter newPosition
  PRE: True
  POST: programCounter with its position changed to newPosition
  SIDE EFFECTS: None
  EXAMPLES:
    setPosition PC ((2, 5), East, False) (3, 6) == PC ((3, 6), East, False)
-}
setPosition :: BProgramCounter -> Position -> BProgramCounter

{- setDirection programCounter newDirection
  PRE: True
  POST: programCounter with its direction changed to newDirection
  SIDE EFFECTS: None
  EXAMPLES:
    setDirection PC ((2, 5), East, False) North = PC ((2, 5), North, False)
-}
setDirection :: BProgramCounter -> Direction -> BProgramCounter

{- getDirection programCounter
  PRE: True
  POST: programCounter's current direction
  SIDE EFFECTS: None
  EXAMPLES:
    getDirection PC ((2, 5), East, False) == East
-}
getDirection :: BProgramCounter -> Direction

{- isStringMode programCounter
  PRE: True
  POST: programCounter's current stringMode
  SIDE EFFECTS: None
  EXAMPLES:
   isStringMode PC ((2, 5), East, False) == False
-}
isStringMode :: BProgramCounter -> StringMode

{- setStringMode programCounter newStringMode
  PRE: True
  POST: programCounter with its stringMode set to newStringMode
  SIDE EFFECTS: None
  EXAMPLES:
    setStringMode PC ((2, 5), East, False) True == PC ((2, 5), East, True)
-}
setStringMode :: BProgramCounter -> StringMode -> BProgramCounter

{- REPRESENTATION CONVENTION:
      - PC (p, d, s) where p is the zero-indexed position the counter is located at
                           d is the direction the counter is facing
                           s is True if the befunge program is in StringMode and False otherwise.

   REPRESENTATION INVARIANT:
      p = (x, y), it must be true that:
         - 0 <= x < width
         - 0 <= y < height

-}
newtype BProgramCounter = PC (Position, Direction, StringMode) deriving (Show)

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
