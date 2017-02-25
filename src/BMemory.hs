module BMemory (BMemory, buildMemory, getValue, putValue) where

import Data.Array.IO
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- buildMemory array lines
  PRE: True
  POST: None
  SIDE EFFECTS: character at line y and column x is added to the array at index (x, y)
  EXAMPLES:
    buildMemory arr ["v>", "<^"] = arr with 'v' at index (0, 0), '>' at index (1, 0) etc.
-}
buildMemory :: BMemory -> [String] -> IO ()

{- getValue array position
  PRE: tuple (mod x width, mod y height) is a valid index in array, where x and y are the x and y coordinate of position
  POST: the value in array at index (mod x width, mod y height)
  SIDE EFFECTS: None
  EXAMPLES:
    (assuming width = 80 and height = 25)
    getValue arr (3, 5) == the value found in arr at index (3, 5)
    getValue arr (82, 56) == the value found in arr at index (2, 6)
-}
getValue :: BMemory -> Position -> IO Char

{- putValue arr position value
  PRE: tuple (mod x width, mod y height) is a valid index in array, where x and y are the x and y coordinate of position
  POST: None
  SIDE EFFECTS: arr with value at index (mod x width, mod y height), where x and y are the x and y coordinate of position
  EXAMPLES:
    putValue arr (3, 5) '+' == places the character '+' at index (3, 5) in arr
-}
putValue :: BMemory -> Position -> Char -> IO ()

{- REPRESENTATION CONVENTION:
      BMemory is an array which represents the grid the befunge program is operating on.

   REPRESENTATION INVARIANT: True

-}
type BMemory = IOArray Position Char

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

buildMemory arr progLines = do
  buildArrayLine progLines 0
  return ()
    where
      buildArrayLine :: [String] -> Int -> IO ()
      buildArrayLine [] _ = return ()
      buildArrayLine (s:ss) y
         | y >= height = return ()
         | otherwise = do
            buildArrayChar (0, y) s
            buildArrayLine ss (y + 1)

      buildArrayChar :: Position -> String -> IO ()
      buildArrayChar _ "" = return ()
      buildArrayChar (x, y) (c:cs)
         | x >= width = return ()
         | otherwise = do
            putValue arr (x, y) c
            buildArrayChar (x + 1, y) cs


getValue arr (x, y) = readArray arr (mod x width, mod y height)

putValue arr (x, y) = writeArray arr (mod x width, mod y height)
