import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO
import System.Random
import Data.Char

import qualified BStack as BS
import qualified BMemory as BM
import qualified BProgramCounter as BPC
import qualified BInstructions as BI
import Types

----------------------------------------
----------------------------------------

_DEBUG = True

----------------------------------------

--main :: IO ()
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

  let stack = BS.empty
  let pc = BPC.starting

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


runProgram :: BM.BMemory -> BS.BStack -> BProgramCounter -> IO ()
runProgram mem stack pc = do
  let (x, y) = BPC.getPosition pc
  char <- BM.getValue mem (x, y)

  when _DEBUG $ do
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ (show x) ++ ", " ++ (show y) ++ ")"

  if char == '@'
    then do
      putStrLn "Program finished"
      return ()
    else do
      (stack', pc') <- executeInstruction mem stack pc char
      runProgram mem stack' (BPC.step pc')


executeInstruction :: BM.BMemory -> BS.BStack -> BProgramCounter -> Char -> IO (BS.BStack, BProgramCounter)
executeInstruction mem stack pc char = do
  case char of
    '+' -> return $ (BI.add stack, pc)
    '-' -> return $ (BI.subtract stack, pc)
    '*' -> return $ (BI.multiply stack, pc)
    '/' -> return $ (BI.divide stack, pc)
    '.' -> BI.printInt stack pc
    d | isDigit d -> return $ (BI.pushStack stack (digitToInt d), pc)
    _ -> return $ (stack, pc)
