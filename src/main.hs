import Prelude
import System.Environment
import System.Exit
import Control.Monad
import Control.Exception
import Data.Array.IO
import Data.Char

import qualified BStack as BS
import qualified BMemory as BM
import qualified BProgramCounter as BPC
import qualified BInstructions as BI
import qualified Flags as F
import Types
import System.Console.ANSI

----------------------------------------
----------------------------------------

main :: IO ()
main = do

  argv <- getArgs
  case F.parseFlags argv of
     status | F.isDisplayHelp status -> printHelpScreen
            | F.isSyntaxError status -> printFlagError status
            | otherwise              -> go (F.extractOptions status)

  putStrLn ""

printFlagError :: F.FlagStatus -> IO ()
printFlagError sta = do
   putStrLn "Wrong use of arguments! use --help for help"
   putStrLn ("error: " ++ F.extractError sta)

go :: F.Options -> IO ()
go opts = do
   let debug = F.getFlag opts F.Debug False
   let filename = F.getFlagString opts F.Filename
   let input = F.getFlagString opts F.Input

   rawProgram <- readProgram filename debug
   let progLines = lines rawProgram

   (memory, stack, pc) <- initialize progLines

   runProgram memory stack pc debug input

initialize :: [String] -> IO (BM.BMemory, BS.BStack, BPC.BProgramCounter)
initialize progLines = do
  memory <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO BM.BMemory
  BM.buildMemory memory progLines

  return (memory, BS.empty, BPC.starting)


readProgram :: String -> Bool -> IO String
readProgram fname debug = do
  when debug $
    putStrLn $ "DEBUG: Reading file \"" ++ fname ++ "\"..."

  catch (do
    contents <- readFile fname
    evaluate contents)
    ((\_ -> do
      putStrLn $ "Couldn't read file \"" ++ fname ++ "\", exiting..."
      exitFailure) :: SomeException -> IO String)


runProgram :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Bool -> String -> IO ()
runProgram mem stack pc debug input = do
  let (x, y) = BPC.getPosition pc
  char <- BM.getValue mem (x, y)

  when debug $
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ show x ++ ", " ++ show y ++ ") " ++ show stack
--" counter facing " ++ show (BPC.getDirection pc) ++ " StringMode=" ++ show (BPC.isStringMode pc)
  unless (char == '@' && not (BPC.isStringMode pc)) $ do
      (stack', pc', input') <- executeInstruction mem stack pc char input
      runProgram mem stack' (BPC.step pc') debug input'


executeInstruction :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Char -> String -> IO (BS.BStack, BPC.BProgramCounter, String)
executeInstruction mem stack pc char input =
  case char of
    '"' -> return (stack, BI.toggleStringMode pc, input)
    c | BPC.isStringMode pc && isAscii c -> return (BS.push stack (ord c), pc, input)
    d | isDigit d -> return (BI.pushStack stack (digitToInt d), pc, input)
    '+' -> return (BI.add stack, pc, input)
    '-' -> return (BI.subtract stack, pc, input)
    '*' -> return (BI.multiply stack, pc, input)
    '/' -> return (BI.divide stack, pc, input)
    '%' -> return (BI.modulo stack, pc, input)
    '!' -> return (BI.logicalNot stack, pc, input)
    '`' -> return (BI.greaterThan stack, pc, input)
    '^' -> return (stack, BPC.setDirection pc North, input)
    '>' -> return (stack, BPC.setDirection pc East, input)
    'v' -> return (stack, BPC.setDirection pc South, input)
    '<' -> return (stack, BPC.setDirection pc West, input)
    '_' -> do
            let (stack', pc') = BI.ifHorizontal stack pc
            return (stack', pc', input)
    '|' -> do
            let (stack', pc') = BI.ifVertical stack pc
            return (stack', pc', input)
    ':' -> return (BI.duplicate stack, pc, input)
    '\\' -> return (BI.swap stack, pc, input)
    '$' -> return (BI.discard stack, pc, input)
    '#' -> return (stack, BPC.step pc, input)
    '?' -> do
            pc' <- BI.randomDir pc
            return (stack, pc', input)
    '.' -> do
            stack' <- BI.printInt stack
            return (stack', pc, input)
    ',' -> do
            stack' <- BI.printAscii stack
            return (stack', pc, input)
    '&' -> do
            (stack', input') <- BI.readInt stack input
            return (stack', pc, input')
    '~' -> do
            (stack', input') <- BI.readASCII stack input
            return (stack', pc, input')
    'g' -> do
            stack' <- BI.getASCII mem stack
            return (stack', pc, input)
    'p' -> do
            stack' <- BI.putASCII mem stack
            return (stack', pc, input)

    _ -> return (stack, pc, input)

printHelpScreen :: IO ()
printHelpScreen = do
   putStrLn "Befunge-93 Interpreter in Haskell!"
   putStrLn "Execute a befunge program in the command line!"
   putStrLn ""
   putStrLn "use: Main [-options] filename"
   putStrLn "\'filename\' is a path to a text file with befunge code in it."
   putStrLn ""
   putStrLn "Options:"
   putStrLn "   ?, -h, --help             <-> Displays this help screen"
   putStrLn "   -d, --debug               <-> Enables debug mode. Prints every step in the execution"
   putStrLn "   -i string, --input string <-> preloads a buffer from \'string\' as input for the program"
   putStrLn ""
   putStrLn "Example:"
   putStrLn "   Main foo.bf93                       -- Runs the program in foo.txt"
   putStrLn "   Main -d foo.bf93                    -- will also print every step the cursor is taking"
   putStrLn "   Main -i \"some input  \" foo.bf93     -- will also give the program a preloaded input buffer"
   putStrLn "   Main -d -i \"hello world\" foo.bf93   -- does both of the above"
   putStrLn ""
   putStrLn "If the befunge program needs input from the buffer but it is empty, it will prompt the user for more buffer."
   putStrLn ""
   putStrLn "For Befunge-93 specifics, search on google."
