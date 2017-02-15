module BStack (BStack, empty, push, pop, top) where

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

empty :: BStack
push :: BStack -> Int -> BStack
pop :: BStack -> (BStack, Int)
top :: BStack -> Int

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

newtype BStack = BStack [Int] deriving (Show)


empty = BStack []


push (BStack bstack) val = BStack (val : bstack)


pop (BStack []) = (BStack [], 0)
pop (BStack (hd:tl)) = (BStack tl, hd)


top (BStack []) = 0
top (BStack (hd:_)) = hd
