import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO

import BStack
import BMemory
import BInstructionPointer

----------------------------------------

type Position = (Int, Int)

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

  let ip = BInstructionPointer.starting
  let stack = BStack.empty

  runProgram mem stack ip


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


runProgram :: BMemory -> BStack -> BInstructionPointer -> IO ()
runProgram mem stack ip = do
  let (x, y) = BInstructionPointer.getPosition ip
  char <- BMemory.getValue mem (x, y)

  when _DEBUG $ do
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ (show x) ++ ", " ++ (show y) ++ ")"
  
  if char == '@'
    then return ()
    else do
      (stack', ip') <- executeInstruction mem stack ip char
      runProgram mem stack' ip'


executeInstruction :: BMemory -> BStack -> BInstructionPointer -> Char -> IO (BStack, BInstructionPointer)
executeInstruction mem stack ip char = do
  case char of
    '+' -> return $ (instr_add stack, step ip)
    _ -> return $ (stack, step ip)
  where
    step = BInstructionPointer.step


instr_add :: BStack -> BStack
instr_add stack =
  let (stack', b) = BStack.pop stack
      (stack'', a) = BStack.pop stack'
  in BStack.push stack'' (a + b)
