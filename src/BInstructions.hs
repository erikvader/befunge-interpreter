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
   toggleStringMode,
   getASCII,
   putASCII) where

import System.IO
import Data.Char
import System.Random
import Prelude hiding (subtract)
import System.Console.ANSI

import qualified BStack as BS
import qualified BProgramCounter as BPC
import qualified BMemory as BM
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

pushStack        :: BS.BStack -> Int -> BS.BStack
add              :: BS.BStack -> BS.BStack
subtract         :: BS.BStack -> BS.BStack
multiply         :: BS.BStack -> BS.BStack
divide           :: BS.BStack -> BS.BStack
modulo           :: BS.BStack -> BS.BStack
logicalNot       :: BS.BStack -> BS.BStack
greaterThan      :: BS.BStack -> BS.BStack
randomDir        :: BPC.BProgramCounter -> IO BPC.BProgramCounter
ifHorizontal     :: BS.BStack -> BPC.BProgramCounter -> (BS.BStack, BPC.BProgramCounter)
ifVertical       :: BS.BStack -> BPC.BProgramCounter -> (BS.BStack, BPC.BProgramCounter)
duplicate        :: BS.BStack -> BS.BStack
swap             :: BS.BStack -> BS.BStack
discard          :: BS.BStack -> BS.BStack
printInt         :: BS.BStack -> IO BS.BStack
printAscii       :: BS.BStack -> IO BS.BStack
readInt          :: BS.BStack -> String -> IO (BS.BStack, String)
readASCII        :: BS.BStack -> String -> IO (BS.BStack, String)
toggleStringMode :: BPC.BProgramCounter -> BPC.BProgramCounter
getASCII         :: BM.BMemory -> BS.BStack -> IO BS.BStack
putASCII         :: BM.BMemory -> BS.BStack -> IO BS.BStack

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
   chars <- readFromInput (trim input) "Integer"
   if null chars
      then return (stack, "")
      else let (hd, tl) = firstWord chars "" in return (BS.push stack (read hd :: Int), tl)
   where
      firstWord [] acc = (reverse acc, "")
      firstWord (' ':ss) acc = (reverse acc, ss)
      firstWord (s:ss) acc = firstWord ss (s:acc)

      trim (' ':ss) = trim ss
      trim ss = ss

readASCII stack input = do
   chars <- readFromInput input "ASCII"
   if null chars
      then return (stack, "")
      else return (BS.push stack (ord (head chars)), tail chars)

--tar en inputsträng, om den är tom ber den användaren att skriva in ngt
readFromInput :: String -> String -> IO String
readFromInput [] prefix = do
   putStrLn ""
   setSGR [SetColor Foreground Vivid Blue]
   putStr (prefix ++ ">>")
   hFlush stdout
   inp <- getLine
   cursorUpLine 1
   clearFromCursorToScreenEnd
   setSGR [Reset]
   return inp
readFromInput input _ = return input

toggleStringMode pc = BPC.setStringMode pc (not (BPC.isStringMode pc))

getASCII mem stack = do
   let (stack', y) = BS.pop stack
   let (stack'', x) = BS.pop stack'
   val <- BM.getValue mem (x, y)
   return $ BS.push stack'' (ord val)

putASCII mem stack = do
   let (stack', y) = BS.pop stack
   let (stack'', x) = BS.pop stack'
   let (stack''', val) = BS.pop stack''
   BM.putValue mem (x, y) (chr val)
   return stack'''
