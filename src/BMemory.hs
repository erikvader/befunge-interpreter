module BMemory (BMemory, buildMemory, getValue, putValue) where

import Data.Array
import Data.Array.IO
import Types

--type Position = (Int, Int)
type BMemory = IOArray Position Char

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

buildMemory :: IOArray Position Char -> [String] -> IO ()
getValue :: IOArray Position Char -> Position -> IO Char
putValue :: IOArray Position Char -> Position -> Char -> IO ()

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

buildMemory arr progLines = do
  (minpos, maxpos) <- getBounds arr
  buildArrayAux arr progLines minpos maxpos 0
  return ()
    where
      buildArrayAux :: IOArray Position Char -> [String] -> Position -> Position -> Int -> IO ()
      buildArrayAux _ _ (_, miny) (_, maxy) y
        | y < miny || y > maxy = return ()
      buildArrayAux _ [] _ _ _ = return ()
      buildArrayAux arr (s:ss) minpos@(minx, miny) maxpos@(maxx, maxy) y = do
        buildArrayAux2 arr minx maxx (0, y) s
        buildArrayAux arr ss minpos maxpos (y + 1)

      buildArrayAux2 :: IOArray Position Char -> Int -> Int -> Position -> String -> IO ()
      buildArrayAux2 _ minx maxx (x, _) _
        | x < minx || x > maxx = return ()
      buildArrayAux2 _ _ _ _ "" = return ()
      buildArrayAux2 arr minx maxx (x, y) (c:cs) = do
        putValue arr (x, y) c
        buildArrayAux2 arr minx maxx (x + 1, y) cs


getValue arr (x, y) = do
  ((minx, miny), (maxx, maxy)) <- getBounds arr
  let w = maxx - minx + 1
  let h = maxy - miny + 1
  readArray arr (mod x w, mod y h)

putValue arr (x, y) val = do
  ((minx, miny), (maxx, maxy)) <- getBounds arr
  let w = maxx - minx + 1
  let h = maxy - miny + 1
  writeArray arr (mod x w, mod y h) val