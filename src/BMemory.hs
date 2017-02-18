module BMemory (BMemory, buildMemory, getValue, putValue) where

import Data.Array
import Data.Array.IO
import Types

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

buildMemory :: BMemory -> [String] -> IO ()
getValue :: BMemory -> Position -> IO Char
putValue :: BMemory -> Position -> Char -> IO ()

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

buildMemory arr progLines = do
  buildArrayAux arr progLines 0
  return ()
    where

      buildArrayAux :: BMemory -> [String] -> Int -> IO ()
      buildArrayAux _ [] _ = return ()
      buildArrayAux arr (s:ss) y
         | y >= height = return ()
         | otherwise = do
            buildArrayAux2 arr (0, y) s
            buildArrayAux arr ss (y + 1)

      buildArrayAux2 :: BMemory -> Position -> String -> IO ()
      buildArrayAux2 _ _ "" = return ()
      buildArrayAux2 arr (x, y) (c:cs)
         | x >= width = return ()
         | otherwise = do
            putValue arr (x, y) c
            buildArrayAux2 arr (x + 1, y) cs


getValue arr (x, y) = readArray arr (mod x width, mod y height)

putValue arr (x, y) val = writeArray arr (mod x width, mod y height) val
