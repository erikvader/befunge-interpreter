module BInstructions (
   pushStack,
   add,
   subtract,
   multiply,
   divide,
   modulo,
   logicalNot,
   greaterThan,
   randomDir,
   ifHorizontal,
   ifVertical,
   duplicate,
   swap,
   discard,
   printInt,
   printAscii,
   readInt,
   readASCII,
   toggleStringMode) where

import System.IO
import Data.Char
import System.Random
import Prelude hiding (subtract)

import qualified BStack as BS
import qualified BProgramCounter as BPC
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

pushStack   :: BS.BStack -> Int -> BS.BStack
add         :: BS.BStack -> BS.BStack
subtract    :: BS.BStack -> BS.BStack
multiply    :: BS.BStack -> BS.BStack
divide      :: BS.BStack -> BS.BStack
modulo      :: BS.BStack -> BS.BStack
logicalNot  :: BS.BStack -> BS.BStack
greaterThan :: BS.BStack -> BS.BStack
randomDir   :: BProgramCounter -> IO BProgramCounter
ifHorizontal:: BS.BStack -> BProgramCounter -> (BS.BStack, BProgramCounter)
ifVertical  :: BS.BStack -> BProgramCounter -> (BS.BStack, BProgramCounter)
duplicate   :: BS.BStack -> BS.BStack
swap        :: BS.BStack -> BS.BStack
discard     :: BS.BStack -> BS.BStack
printInt    :: BS.BStack -> IO BS.BStack
printAscii  :: BS.BStack -> IO BS.BStack
readInt     :: BS.BStack -> String -> IO BS.BStack
readASCII   :: BS.BStack -> String -> IO BS.BStack
toggleStringMode :: BProgramCounter -> BProgramCounter

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

modulo stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in
    BS.push stack'' (mod a b)

logicalNot stack =
  let (stack', a) = BS.pop stack
  in case a of
    0 -> BS.push stack' 1
    _ -> BS.push stack' 0

greaterThan stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in if a > b
      then BS.push stack'' 1
      else BS.push stack'' 0

randomDir pc = do
   rand <- randomRIO (1, 4::Int)
   return $ BPC.setDirection pc (dir rand)
   where
      dir 1 = South
      dir 2 = North
      dir 3 = West
      dir _ = East

ifHorizontal stack pc =
  let (stack', a) = BS.pop stack
  in case a of
    0 -> (stack', BPC.setDirection pc East)
    _ -> (stack', BPC.setDirection pc West)

ifVertical stack pc =
  let (stack', a) = BS.pop stack
  in case a of
    0 -> (stack', BPC.setDirection pc South)
    _ -> (stack', BPC.setDirection pc North)

duplicate stack = BS.push stack (BS.top stack)

swap stack =
  let (stack', a) = BS.pop stack
      (stack'', b) = BS.pop stack'
  in BS.push (BS.push stack'' a) b

discard stack =
  let (stack', _) = BS.pop stack
  in stack'

printInt stack = do
  let (stack', a) = BS.pop stack
  putStr (show a ++ " ")
  hFlush stdout
  return stack'

printAscii stack = do
  let (stack', a) = BS.pop stack
  putStr [chr a]
  hFlush stdout
  return stack'

readInt stack input = do
   char <- readFromInput input
   if null char
      then return stack
      else return $ BS.push stack (read (firstWord char) :: Int)
   where firstWord s = head $ words s

readASCII stack input = do
   char <- readFromInput input
   if null char
      then return stack
      else return $ BS.push stack (ord (head char))

--tar en inputsträng, om den är tom ber den användaren att skriva in ngt
readFromInput :: String -> IO String
readFromInput [] = do
   putStr ">>"
   hFlush stdout
   getLine
readFromInput input = return input

toggleStringMode pc = BPC.setStringMode pc (not (BPC.isStringMode pc))
