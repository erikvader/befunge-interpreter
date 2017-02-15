module BArray (buildArray) where

import Data.Array.IO

buildArray :: [String] -> IO (IOArray (Int, Int) Char)
buildArray progLines = do
  arr <- newArray ((0, 0), (79, 24)) ' '
  buildArrayAux arr progLines 0
  return arr
    where
      buildArrayAux :: IOArray (Int, Int) Char -> [String] -> Int -> IO ()
      buildArrayAux _ [] _ = return ()
      buildArrayAux arr (s:ss) y 
        | y > 24 = return ()
        | otherwise = do
		      buildArrayAux2 arr (0, y) s
		      buildArrayAux arr ss (y + 1)

      buildArrayAux2 :: IOArray (Int, Int) Char -> (Int, Int) -> String -> IO ()
      buildArrayAux2 arr _ "" = return ()
      buildArrayAux2 arr (x, y) (c:cs)
			| x > 79 = return ()
			| otherwise = do 
        writeArray arr (x, y) c
        buildArrayAux2 arr (x + 1, y) cs


