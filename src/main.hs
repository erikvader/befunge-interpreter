import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO
import System.Random

import qualified BStack as BS
import qualified BMemory as BM
import Types

----------------------------------------
width = 80
height = 25
----------------------------------------

_DEBUG = True

----------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  let argc = length argv

  when (argc == 0) $ do
    putStrLn "Missing argument filename, exiting"
    exitFailure

  when (argc > 1) $ do
    putStrLn "Too many arguments, ignoring..."

  let filename = argv !! 0
  program <- readProgram filename
  let progLines = lines program

  mem <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO (BM.BMemory)
  BM.buildMemory mem progLines

  let pc = PC ((0, 0), East)
  let stack = BS.empty

  runProgram mem stack pc


readProgram :: String -> IO String
readProgram fname = do
  when _DEBUG $ do
    putStrLn $ "DEBUG: Reading file \"" ++ fname ++ "\"..."

  catch (do
    contents <- readFile fname
    evaluate contents)
    ((\_ -> do
      putStrLn $ "Couldn't read file \"" ++ fname ++ "\", exiting..."
      exitFailure) :: SomeException -> IO String)


runProgram :: BM.BMemory -> BS.BStack -> BProgramCursor -> IO ()
runProgram mem stack pc@(PC ((x, y), dir)) = do
  char <- BM.get mem (x, y)

  when _DEBUG $ do
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ (show x) ++ ", " ++ (show y) ++ ")"

  if char == '@'
    then do
      putStrLn "Program finished"
      return ()
    else do
      (stack', pc') <- executeInstruction mem stack pc char
      runProgram mem stack' pc'



executeInstruction :: BM.BMemory -> BS.BStack -> BProgramCursor -> Char -> IO (BS.BStack, BProgramCursor)
executeInstruction mem stack pc char = do
  case char of
    '+' -> return $ (instr_add stack, step pc)
    _ -> return $ (stack, step pc)
  where
    step :: BProgramCursor -> BProgramCursor
    step (PC ((x, y), North)) = PC ((x, mod (y - 1) height), North)
    step (PC ((x, y), East)) = PC ((mod (x + 1) width, y), East)
    step (PC ((x, y), South)) = PC ((x, mod (y + 1) height), South)
    step (PC ((x, y), West)) = PC ((mod (x - 1) width, y), West)


instr_add :: BS.BStack -> BS.BStack
instr_add stack =
  let (stack', b) = BS.pop stack
      (stack'', a) = BS.pop stack'
  in BS.push stack'' (a + b)
