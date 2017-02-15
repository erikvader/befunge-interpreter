import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO

import BStack
import BMemory
import BProgramCounter
import BInstructions

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

  let ip = BProgramCounter.starting
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


runProgram :: BMemory -> BStack -> BProgramCounter -> IO ()
runProgram mem stack ip = do
  let (x, y) = BProgramCounter.getPosition ip
  char <- BMemory.getValue mem (x, y)

  when _DEBUG $ do
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ (show x) ++ ", " ++ (show y) ++ ")"
  
  if char == '@'
    then return ()
    else do
      (stack', ip') <- executeInstruction mem stack ip char
      runProgram mem stack' ip'


executeInstruction :: BMemory -> BStack -> BProgramCounter -> Char -> IO (BStack, BProgramCounter)
executeInstruction mem stack ip char = do
  case char of
    '+' -> return $ (BInstructions.add stack, ip')
    '-' -> return $ (BInstructions.subtract stack, ip')
    '*' -> return $ (BInstructions.multiply stack, ip')
    '/' -> return $ (BInstructions.divide stack, ip')
    '.' -> do
      stack' <- BInstructions.printInt stack
      return (stack, ip')
    '1' -> return $ (BInstructions.pushStack stack 1, ip')
    '2' -> return $ (BInstructions.pushStack stack 2, ip')
    '3' -> return $ (BInstructions.pushStack stack 3, ip')
    '4' -> return $ (BInstructions.pushStack stack 4, ip')
    '5' -> return $ (BInstructions.pushStack stack 5, ip')
    '6' -> return $ (BInstructions.pushStack stack 6, ip')
    '7' -> return $ (BInstructions.pushStack stack 7, ip')
    '8' -> return $ (BInstructions.pushStack stack 8, ip')
    '9' -> return $ (BInstructions.pushStack stack 9, ip')
    _ -> return $ (stack, step ip)
  where
    ip' = BProgramCounter.step ip
