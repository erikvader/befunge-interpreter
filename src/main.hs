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

   rawProgram <- readProgram filename debug
   let progLines = lines rawProgram

   (memory, stack, pc) <- initialize progLines

   runProgram memory stack pc debug

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


runProgram :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Bool -> IO ()
runProgram mem stack pc debug = do
  let (x, y) = BPC.getPosition pc
  char <- BM.getValue mem (x, y)

  when debug $
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ show x ++ ", " ++ show y ++ ") " ++ show stack

  unless (char == '@' && not (BPC.isStringMode pc)) $ do
      (stack', pc') <- executeInstruction mem stack pc char
      runProgram mem stack' (BPC.step pc') debug


executeInstruction :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Char -> IO (BS.BStack, BPC.BProgramCounter)
executeInstruction mem stack pc char =
  case char of
    '"' -> return (stack, BI.toggleStringMode pc)
    c | BPC.isStringMode pc && isAscii c -> return (BS.push stack (ord c), pc)
    d | isDigit d -> return (BI.pushStack stack (digitToInt d), pc)
    '+' -> return (BI.add stack, pc)
    '-' -> return (BI.subtract stack, pc)
    '*' -> return (BI.multiply stack, pc)
    '/' -> return (BI.divide stack, pc)
    '%' -> return (BI.modulo stack, pc)
    '!' -> return (BI.logicalNot stack, pc)
    '`' -> return (BI.greaterThan stack, pc)
    '^' -> return (stack, BPC.setDirection pc North)
    '>' -> return (stack, BPC.setDirection pc East)
    'v' -> return (stack, BPC.setDirection pc South)
    '<' -> return (stack, BPC.setDirection pc West)
    '_' -> do
            let (stack', pc') = BI.ifHorizontal stack pc
            return (stack', pc')
    '|' -> do
            let (stack', pc') = BI.ifVertical stack pc
            return (stack', pc')
    ':' -> return (BI.duplicate stack, pc)
    '\\' -> return (BI.swap stack, pc)
    '$' -> return (BI.discard stack, pc)
    '#' -> return (stack, BPC.step pc)
    '?' -> do
            pc' <- BI.randomDir pc
            return (stack, pc')
    '.' -> do
            stack' <- BI.printInt stack
            return (stack', pc)
    ',' -> do
            stack' <- BI.printAscii stack
            return (stack', pc)
    '&' -> do
            stack' <- BI.readInt stack
            return (stack', pc)
    '~' -> do
            stack' <- BI.readASCII stack
            return (stack', pc)
    'g' -> do
            stack' <- BI.getASCII mem stack
            return (stack', pc)
    'p' -> do
            stack' <- BI.putASCII mem stack
            return (stack', pc)

    _ -> return (stack, pc)

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
   putStrLn ""
   putStrLn "Example:"
   putStrLn "   Main foo.bf93                       -- Runs the program in foo.txt"
   putStrLn "   Main -d foo.bf93                    -- will also print every step the cursor is taking"
   putStrLn ""
   putStrLn "For Befunge-93 specifics, search on google."
