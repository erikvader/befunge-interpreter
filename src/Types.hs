module Types (Position, Direction(North, East, South, West), BProgramCounter(PC), width, height, BMemory) where

import Data.Array.IO

type Position = (Int, Int)
data Direction = North | East | South | West deriving Show
newtype BProgramCounter = PC (Position, Direction) deriving (Show)
width = 80 :: Int
height = 25 :: Int
type BMemory = IOArray Position Char
