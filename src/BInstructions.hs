module BInstructions (
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
   printASCII,
   readInt,
   readASCII,
   toggleStringMode,
   getASCII,
   putASCII) where

import System.IO
import Data.Char
import System.Random
import Prelude hiding (subtract)

import qualified BStack as BS
import qualified BProgramCounter as BPC
import qualified BMemory as BM
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- add s
   PRE: True
   POST: pops the first two elements from s and pushes their sum to s
-}
add :: BS.BStack -> BS.BStack

{- subtract s
   PRE: True
   POST: pops b from s, then pops a from s and finally pushes (a - b) to s
-}
subtract :: BS.BStack -> BS.BStack

{- multiply s
   PRE: True
   POST: pops the first two elements from s and pushes their product to s
-}
multiply :: BS.BStack -> BS.BStack

{- divide s
   PRE: True
   POST: pops b from s, then pops a from s and finally pushes (a `div` b) to s
         If b would be 0, then 0 is pushed to s instead.
-}
divide :: BS.BStack -> BS.BStack

{- modulo s
   PRE: True
   POST: pops b from s, then pops a from s and finally pushes (a `mod` b) to s
         If b would be 0, then 0 is pushed to s instead.
-}
modulo :: BS.BStack -> BS.BStack

{- logicalNot s
   PRE: True
   POST: pops a from s and pushes:
            1 if a == 0
            0 if a /= 0
         back to s
-}
logicalNot :: BS.BStack -> BS.BStack

{- greaterThan s
   PRE: True
   POST: first pops b then a from s and pushes:
            1 if a > b
            0 if a <= b
         back to s
-}
greaterThan :: BS.BStack -> BS.BStack

{- randomDir pc
   PRE: True
   POST: pc but with a random Direction
   SIDE EFFECTS: takes a random number from randomRIO
-}
randomDir :: BPC.BProgramCounter -> IO BPC.BProgramCounter

{- ifHorizontal s pc
   PRE: True
   POST: (s', pc') where s' is s but without its top element, and pc' is pc but
         its direction is set to:
            East if the top element in s is 0
            West otherwise
-}
ifHorizontal :: BS.BStack -> BPC.BProgramCounter -> (BS.BStack, BPC.BProgramCounter)

{- ifVertical s pc
   PRE: True
   POST: (s', pc') where s' is s but without its top element, and pc' is pc but
         its direction is set to:
            South if the top element in s is 0
            North otherwise
-}
ifVertical :: BS.BStack -> BPC.BProgramCounter -> (BS.BStack, BPC.BProgramCounter)

{- duplicate s
   PRE: True
   POST: s with its top element pushed again
-}
duplicate :: BS.BStack -> BS.BStack

{- swap s
   PRE: True
   POST: s but with its first two elements swapped
-}
swap :: BS.BStack -> BS.BStack

{- discard s
   PRE: True
   POST: s without its first element
-}
discard :: BS.BStack -> BS.BStack

{- printInt s
   PRE: True
   POST: s without its top element
   SIDE EFFECTS: prints out the top element in s as a number and a space to stdout
   EXAMPLES: printInt (BStack [65]) = IO (BStack []) : and prints '65 '
-}
printInt :: BS.BStack -> IO BS.BStack

{- printASCII s
   PRE: True
   POST: s without its top element
   SIDE EFFECTS: prints out the top element in s converted to ASCII to stdout
   EXAMPLES: printASCII (BStack [65]) = IO (BStack []) : and prints 'A'
-}
printASCII :: BS.BStack -> IO BS.BStack

{- readInt s
   PRE: True
   POST: s with a number read from stdin pushed to it. If the input can't be
         parsed to a number, 0 is pushed to s instead.
   SIDE EFFECTS: prints a prompt to stdout and waits for input on stdin
-}
readInt :: BS.BStack -> IO BS.BStack

{- readASCII s
   PRE: True
   POST: s with the first character from stdin pushed as its ASCII-value.
         If the input was empty, 0 is pushed to s instead.
   SIDE EFFECTS: prints a prompt to stdout and waits for input on stdin
-}
readASCII :: BS.BStack -> IO BS.BStack

{- readFromInput s
   PURPOSE: helper function to readASCII and readInt
   PRE: True
   POST: raw input from stdin as a String
   SIDE EFFECTS: prompts the user with (s ++ ">>") and waits for input in stdin
-}
readFromInput :: String -> IO String

{- toggleStringMode pc
   PRE: True
   POST: pc with its StringMode set to True if it was False, and False if it was True.
   EXAMPLES: examples
-}
toggleStringMode :: BPC.BProgramCounter -> BPC.BProgramCounter

{- getASCII m s
   PRE: True
   POST: the first two elements from s are popped where the first one is y and the
         second one is x. s is returned with the ASCII-value stored in m at position
         (x, y) pushed to s.
-}
getASCII :: BM.BMemory -> BS.BStack -> IO BS.BStack

{- putASCII m s
   PRE: True
   POST: y, x and c is popped from s in order
   SIDE EFFECTS: the character at position (x, y) in m is set to the character c converted to ASCII.
-}
putASCII :: BM.BMemory -> BS.BStack -> IO BS.BStack

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

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
  in case b of
    0 -> BS.push stack'' 0
    _ -> BS.push stack'' (mod a b)

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

printASCII stack = do
  let (stack', a) = BS.pop stack
  putStr [chr a]
  hFlush stdout
  return stack'

readInt stack = do
   chars <- readFromInput "Int"
   if not (isInteger chars)
      then return $ BS.push stack 0
      else return $ BS.push stack (read chars :: Int)
   where
      isInteger :: String -> Bool
      isInteger []     = False
      isInteger ('-':ss) = onlyDigits ss
      isInteger ss = onlyDigits ss

      onlyDigits :: String -> Bool
      onlyDigits [] = False
      onlyDigits s  = foldl (\b h -> b && isDigit h) True s

readASCII stack = do
   chars <- readFromInput "Char"
   if null chars
      then return $ BS.push stack 0
      else return $ BS.push stack (ord (head chars))

readFromInput prefix = do
   putStr (prefix ++ ">>")
   hFlush stdout
   getLine

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
