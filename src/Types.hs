module Types (Position, Direction(North, East, South, West), BProgramCounter(PC), width, height, BMemory, StringMode) where

import Data.Array.IO

type Position = (Int, Int)
data Direction = North | East | South | West deriving Show
type StringMode = Bool
newtype BProgramCounter = PC (Position, Direction, StringMode) deriving (Show) --känns inte bra att denna är här, exposar PC för alla.
width = 80 :: Int
height = 25 :: Int
type BMemory = IOArray Position Char
