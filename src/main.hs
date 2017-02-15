import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO

import BStack
import BMemory

----------------------------------------

type Position = (Int, Int)
data Direction = North | East | South | West
newtype BProgramCursor = PC (Position, Direction)

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

  mem <- newArray ((0, 0), (79, 24)) ' ' :: IO (BMemory)
  buildMemory mem progLines

  let pc = PC ((0, 0), East)
  let stack = BStack.empty

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


runProgram :: BMemory -> BStack -> BProgramCursor -> IO ()
runProgram mem stack pc@(PC ((x, y), dir)) = do
  char <- BMemory.get mem (x, y)

  when _DEBUG $ do
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ (show x) ++ ", " ++ (show y) ++ ")"
  
  if char == '@'
    then return ()
    else do
      (stack', pc') <- executeInstruction mem stack pc char
      runProgram mem stack' pc'



executeInstruction :: BMemory -> BStack -> BProgramCursor -> Char -> IO (BStack, BProgramCursor)
executeInstruction mem stack pc char = do
  case char of
    '+' -> return $ (instr_add stack, step pc)
    _ -> return $ (stack, step pc)
  where
    step :: BProgramCursor -> BProgramCursor
    step (PC ((x, y), North)) = PC ((x, y - 1), North)
    step (PC ((x, y), East)) = PC ((x + 1, y), East)
    step (PC ((x, y), South)) = PC ((x, y + 1), South)
    step (PC ((x, y), West)) = PC ((x - 1, y), West)


instr_add :: BStack -> BStack
instr_add stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a + b)










