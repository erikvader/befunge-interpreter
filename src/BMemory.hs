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
  buildArrayAux arr progLines 0
  return ()
    where
      buildArrayAux :: IOArray Position Char -> [String] -> Int -> IO ()
      buildArrayAux _ _ y | y == height = error $ "Too many lines"
      buildArrayAux _ [] _ = return ()
      buildArrayAux arr (s:ss) y = do
        buildArrayAux2 arr (0, y) s
        buildArrayAux arr ss (y + 1)

      buildArrayAux2 :: IOArray Position Char -> Position -> String -> IO ()
      buildArrayAux2 _ (x, y) _ | x == width = error $ "Too many characters on line " ++ show (y)
      buildArrayAux2 _ _ "" = return ()
      buildArrayAux2 arr (x, y) (c:cs) = do
        putValue arr (x, y) c
        buildArrayAux2 arr (x + 1, y) cs


getValue arr (x, y) =
  if x < 0 || x >= width || y < 0 || y >= height
    then return ' ' --allow illegal get / put? Or raise error?
    else readArray arr (x, y)

putValue arr (x, y) val = 
  if x < 0 || x >= width || y < 0 || y >= height
    then return ()
    else writeArray arr (x, y) val