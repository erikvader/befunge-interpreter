module BInstructions (
   pushStack,
   add,
   subtract,
   multiply,
   divide,
   printInt,
   printAscii,
   duplicate,
   readInt,
   readASCII,
   randomDir) where

import System.IO
import Data.Char
import System.Random
import Prelude hiding (subtract)

import qualified BMemory as BM
import qualified BStack as BS
import qualified BProgramCounter as BPC
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

pushStack  :: BS.BStack -> Int -> BS.BStack
add        :: BS.BStack -> BS.BStack
subtract   :: BS.BStack -> BS.BStack
multiply   :: BS.BStack -> BS.BStack
divide     :: BS.BStack -> BS.BStack
printInt   :: BS.BStack -> IO BS.BStack
printAscii :: BS.BStack -> IO BS.BStack
duplicate  :: BS.BStack -> BS.BStack
readInt    :: BS.BStack -> String -> IO (BS.BStack)
readASCII  :: BS.BStack -> String -> IO (BS.BStack)
randomDir  :: BProgramCounter -> IO (BProgramCounter)

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

duplicate stack = BS.push stack (BS.top stack)

readInt stack input = do
   char <- readFromInput input
   return $ BS.push stack (digitToInt char)

readASCII stack input = do
   char <- readFromInput input
   return $ BS.push stack (ord char)

--tar en inputsträng, om den är tom ber den användaren att skriva in ngt
readFromInput [] = do
   putStr ">>"
   inp <- getLine
   return $ head inp
readFromInput input = return $ head input

randomDir pc = do
   rand <- randomRIO (1, 4::Int)
   return $ BPC.setDirection pc (dir rand)
   where
      dir 1 = South
      dir 2 = North
      dir 3 = West
      dir 4 = East
