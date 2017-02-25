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

{- main
   PURPOSE: runs the program by first parsing the arguments given to the program
            and doing an appropiate action.
-}
main :: IO ()
main = do

  argv <- getArgs
  case F.parseFlags argv of
     status | F.isDisplayHelp status -> printHelpScreen
            | F.isSyntaxError status -> printFlagError status
            | otherwise              -> go (F.extractOptions status)

  putStrLn ""

{- printFlagError f
   PRE: f is SyntaxError e
   POST: none
   SIDE EFFECTS: prints out the error message e
-}
printFlagError :: F.FlagStatus -> IO ()
printFlagError sta = do
   putStrLn "Wrong use of arguments! use --help for help"
   putStrLn ("error: " ++ F.extractError sta)

{- go o
   PURPOSE: there were no parse errors on the arguments given to the program,
            so we can continue to run the rest of the program
   PRE: True
-}
go :: F.Options -> IO ()
go opts = do
   let debug = F.getFlag opts F.Debug False
   let filename = F.getFlagString opts F.Filename

   rawProgram <- readProgram filename debug
   let progLines = lines rawProgram

   (memory, stack, pc) <- initialize progLines

   runProgram memory stack pc debug

{- initialize ss
   PRE: True
   POST: (m, s, pc) where m is a new array filled with characters from ss,
         s is the empty stack and pc is the default counter.
   SIDE EFFECTS: creates an IOArray
-}
initialize :: [String] -> IO (BM.BMemory, BS.BStack, BPC.BProgramCounter)
initialize progLines = do
  memory <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO BM.BMemory
  BM.buildMemory memory progLines

  return (memory, BS.empty, BPC.starting)

{- readProgram s b
   PRE: True
   POST: the contents from the file s is pointing to.
   SIDE EFFECTS: if b is True, a debug message is printed out. If the reading
                 of the file somwhow failes, the program terminates.
-}
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


{- runProgram m s pc b
   PURPOSE: the main loop of the program.
   PRE: True
   POST: None
   SIDE EFFECTS: if b is True then debug messages will be printed to stdout.
                 m might be changed.
-}
runProgram :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Bool -> IO ()
runProgram mem stack pc debug = do
  let (x, y) = BPC.getPosition pc
  char <- BM.getValue mem (x, y)

  when debug $
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ show x ++ ", " ++ show y ++ ") " ++ show stack

  unless (char == '@' && not (BPC.isStringMode pc)) $ do
      (stack', pc') <- executeInstruction mem stack pc char
      runProgram mem stack' (BPC.step pc') debug


{- executeInstruction m s pc c
   PRE: True
   POST: (s', pc') where s' and pc' are the result of applying a function depending
         on c.
   SIDE EFFECTS: m might be changed. Output might be printed to stdout. 
-}
executeInstruction :: BM.BMemory -> BS.BStack -> BPC.BProgramCounter -> Char -> IO (BS.BStack, BPC.BProgramCounter)
executeInstruction mem stack pc char =
  case char of
    '"' -> return (stack, BI.toggleStringMode pc)
    c | BPC.isStringMode pc && isAscii c -> return (BS.push stack (ord c), pc)
    d | isDigit d -> return (BS.push stack (digitToInt d), pc)
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
            stack' <- BI.printASCII stack
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

{- printHelpScreen
   PRE: True
   POST: None
   SIDE EFFECTS: prints a help screen to stdout
-}
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
