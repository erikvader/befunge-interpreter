module BInstructions (pushStack, add, subtract, multiply, divide, printInt) where

import System.IO
import Prelude hiding (subtract)

import BMemory
import BStack
import BProgramCounter

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

pushStack :: BStack -> Int -> BStack
add :: BStack -> BStack
subtract :: BStack -> BStack
multiply :: BStack -> BStack
divide :: BStack -> BStack
printInt :: BStack -> IO (BStack)

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

pushStack = BStack.push

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


printInt stack = do
  let (stack', a) = BStack.pop stack
  putStr (show a ++ " ")
  return stack'
