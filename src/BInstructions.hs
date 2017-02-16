module BInstructions (pushStack, add, subtract, multiply, divide, printInt) where

import System.IO
import Prelude hiding (subtract)

import qualified BMemory as BM
import qualified BStack as BS
import qualified BProgramCounter as BPC
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

pushStack :: BS.BStack -> Int -> BS.BStack
add :: BS.BStack -> BS.BStack
subtract :: BS.BStack -> BS.BStack
multiply :: BS.BStack -> BS.BStack
divide :: BS.BStack -> BS.BStack
printInt :: BS.BStack -> BProgramCounter -> IO (BS.BStack, BProgramCounter)

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

pushStack = BS.push

add stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in BS.push stack'' (a + b)


subtract stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in BS.push stack'' (a - b)


multiply stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in BS.push stack'' (a * b)


divide stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in BS.push stack'' (a `div` b)


printInt stack pc = do
  let (stack', a) = BS.pop stack
  putStr (show a ++ " ")
  return (stack', pc)
