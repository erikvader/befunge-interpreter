module Types (Position, Direction(North, East, South, West), width, height, BMemory, StringMode) where

import Data.Array.IO

type Position = (Int, Int)
data Direction = North | East | South | West deriving Show
type StringMode = Bool
--newtype BProgramCounter = PC (Position, Direction, StringMode) deriving (Show) --känns inte bra att denna är här, exposar PC för alla.
type BMemory = IOArray Position Char --ska kanske också flyttas

width :: Int
width = 80

height :: Int
height = 25
