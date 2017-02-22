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

newtype BStack = BStack [Int]

instance Show BStack where
   show (BStack s) = "Stack: "++ show (reverse s)

empty = BStack []


--push bs@(BStack []) 0 = bs
push (BStack bstack) val = BStack (val : bstack)


pop (BStack []) = (BStack [], 0)
pop (BStack (hd:tl)) = (BStack tl, hd)


top (BStack []) = 0
top (BStack (hd:_)) = hd
