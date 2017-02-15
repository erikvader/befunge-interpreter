module BMemory (BMemory, buildMemory, get, put) where

import Data.Array
import Data.Array.IO
import Types

--type Position = (Int, Int)
type BMemory = IOArray Position Char

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

buildMemory :: IOArray Position Char -> [String] -> IO ()
get :: IOArray Position Char -> Position -> IO Char
put :: IOArray Position Char -> Position -> Char -> IO ()

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

buildMemory arr progLines = do
  buildArrayAux arr progLines 0
  return ()
    where
      buildArrayAux :: IOArray Position Char -> [String] -> Int -> IO ()
      buildArrayAux _ [] _ = return ()
      buildArrayAux arr (s:ss) y = do
        buildArrayAux2 arr (0, y) s
        buildArrayAux arr ss (y + 1)

      buildArrayAux2 :: IOArray Position Char -> Position -> String -> IO ()
      buildArrayAux2 arr _ "" = return ()
      buildArrayAux2 arr (x, y) (c:cs) = do
        put arr (x, y) c
        buildArrayAux2 arr (x + 1, y) cs


get arr (x, y) = do
  (minpos, maxpos) <- getBounds arr
  if (x, y) < minpos || (x, y) > maxpos
    then return ' '
    else readArray arr (x, y)

put arr (x, y) val = do
  (minpos, maxpos) <- getBounds arr
  if (x, y) < minpos || (x, y) > maxpos
    then return ()
    else writeArray arr (x, y) val
