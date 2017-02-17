module BInstructions (pushStack, add, subtract, multiply, divide, printInt, printAscii, duplicate) where

import System.IO
import Data.Char
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
printInt :: BS.BStack -> IO BS.BStack
printAscii :: BS.BStack -> IO BS.BStack
duplicate :: BS.BStack -> BS.BStack

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
  in case b of
    0 -> BS.push stack'' 0
    _ -> BS.push stack'' (a `div` b)


printInt stack = do
  let (stack', a) = BS.pop stack
  putStr ((show a) ++ " ")
  return stack'

printAscii stack = do
  let (stack', a) = BS.pop stack
  putStr ([chr a])
  return stack'

duplicate stack = BS.push stack (snd $ BS.pop stack) -- Veto on not using BS.top by Patrik