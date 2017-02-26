module BStack (BStack, empty, push, pop, top) where

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

--the empty stack
empty :: BStack

{- push stack value
  PRE: True
  POST: The stack with value as the first element
  SIDE EFFECTS: None
  EXAMPLES:
    push empty 65 == BStack [65]
    push (BStack [65]) 66 == BStack [66, 65]
-}
push :: BStack -> Int -> BStack

{- pop stack
  PRE: True
  POST: The tuple (stack without the first element, the first element of stack)
  SIDE EFFECTS: None
  EXAMPLES:
    pop empty == (BStack [], 0)
    pop (BStack [66, 65]) == (BStack [65], 66)
-}
pop :: BStack -> (BStack, Int)

{- top stack
  PRE: True
  POST: The first element of stack
  SIDE EFFECTS: None
  EXAMPLES:
    top empty == 0
    top (BStack [66, 65]) == 66
-}
top :: BStack -> Int

{- REPRESENTATION CONVENTION:
      - BStack l where the first element in l is the top of the stack.
      - BStack [] represents a stack with infinitely many zeros.

   REPRESENTATION INVARIANT: BStack l: l can not contain only zeros.
-}
newtype BStack = BStack [Int] deriving (Eq)

instance Show BStack where
   show (BStack s) = "Stack: " ++ show (reverse s)

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

empty = BStack []

push bs@(BStack []) 0 = bs
push (BStack bstack) val = BStack (val : bstack)

pop (BStack []) = (BStack [], 0)
pop (BStack (hd:tl)) = (BStack tl, hd)

top (BStack []) = 0
top (BStack (hd:_)) = hd
