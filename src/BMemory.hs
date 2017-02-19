module BMemory (BMemory, buildMemory, getValue, putValue) where

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
