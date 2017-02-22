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
--_DEBUG :: Bool
--_DEBUG = True

----------------------------------------

main :: IO ()
main = do
  argv <- getArgs

  case F.parseFlags argv of
     status | F.isDisplayHelp status -> putStrLn "du behöver hjälp!"
            | F.isSyntaxError status -> putStrLn "du skrev fel! --help för hjälp"
            | otherwise              -> go (F.extractOptions status)


{-let argc = length argv

when (argc == 0) $ do
  putStrLn "Missing argument filename, exiting" --displayhelp
  exitFailure

when (argc > 1) $
  putStrLn "Too many arguments, ignoring..."

let filename = head argv-}

go :: F.Options -> IO ()
go opts = do
   let debug = F.getFlag opts F.Debug False
   let filename = F.getFlag opts F.Filename ""

   rawProgram <- readProgram filename debug
   let progLines = lines rawProgram

   (memory, stack, pc) <- initialize progLines

   runProgram memory stack pc debug

initialize :: [String] -> IO (BMemory, BS.BStack, BPC.BProgramCounter)
initialize progLines = do
  memory <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO BMemory
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
    putStrLn $ "DEBUG: Read character '" ++ char : "' at position (" ++ show x ++ ", " ++ show y ++ ")" ++ " counter facing " ++ show (BPC.getDirection pc) ++ " StringMode=" ++ show (BPC.isStringMode pc)

  unless (char == '@') $ do
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
    '_' -> return $ BI.ifHorizontal stack pc
    '|' -> return $ BI.ifVertical stack pc
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
            stack' <- BI.readInt stack []
            return (stack', pc)
    '~' -> do
            stack' <- BI.readASCII stack []
            return (stack', pc)
    'g' -> do
            stack' <- BI.getASCII mem stack
            return (stack', pc)
    'p' -> do
            stack' <- BI.putASCII mem stack
            return (stack', pc)

    _ -> return (stack, pc)
