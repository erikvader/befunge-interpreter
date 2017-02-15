module BInstructions (add, subtract) where

import BMemory
import BStack
import BInstructionPointer

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

add :: BStack -> BStack
subtract :: BStack -> BStack
multiply :: BStack -> BStack
divide :: BStack -> BStack

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

add stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a + b)


subtract stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a - b)


multiply stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a * b)


divide stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a `div` b)