module Flags(
   Options,
   Flag(Debug, Help, Filename, Input, Width, Height),
   FlagStatus,
   parseFlags,
   getFlag,
   getFlagString,
   isSyntaxError,
   isDisplayHelp,
   extractOptions,
   extractError) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Char

data Flag = Debug | Help | Filename | Input | Width | Height deriving (Show, Eq)
type Options = HM.HashMap Flag String
data FlagStatus = NoError Options | DisplayHelp | SyntaxError String

instance Hashable Flag where
   hashWithSalt a f = hashWithSalt a (show f)

getFlagFromString :: String -> Maybe Flag
getFlagFromString "-d"       = Just Debug
getFlagFromString "--debug"  = Just Debug
getFlagFromString "--help"   = Just Help
getFlagFromString "-h"       = Just Help
getFlagFromString "?"        = Just Help
getFlagFromString "-i"       = Just Input
getFlagFromString "--input"  = Just Input
getFlagFromString "--width"  = Just Width
getFlagFromString "--height" = Just Height
getFlagFromString _          = Nothing

parseFlags :: [String] -> FlagStatus
parseFlags [] = SyntaxError "no flags found"
parseFlags s = parse s HM.empty

parse :: [String] -> Options -> FlagStatus
parse [] opt = NoError opt
parse (s:ss) opt =
   case getFlagFromString s of
      (Just Height)     -> parseInt ss opt Height
      (Just Width)      -> parseInt ss opt Width
      (Just Debug)      -> parse ss (HM.insert Debug "True" opt)
      (Just Help)       -> DisplayHelp
      (Just Input)      -> parseString ss opt Input
      Nothing | null ss -> parse ss (HM.insert Filename s opt)
      Nothing           -> SyntaxError ("'"++ s ++ "'" ++ " is not a valid flag")

parseString :: [String] -> Options -> Flag -> FlagStatus
parseString [] _ f       = SyntaxError ("'" ++ show f ++ "' is missing an argument")
parseString (s:ss) opt f = parse ss (HM.insert f s opt)

parseInt :: [String] -> Options -> Flag -> FlagStatus
parseInt [] opt f = SyntaxError ("'" ++ show f ++ "' is missing an argument")
parseInt (s:ss) opt f
   | isInteger s = parse ss (HM.insert f s opt)
   | otherwise   = SyntaxError ("'"++ s ++ "'" ++ " is not a positive integer")

isInteger :: String -> Bool
isInteger []     = True
isInteger (s:ss) = isDigit s && isInteger ss

getFlag :: (Read v) => Options -> Flag -> v -> v
getFlag opt f def =
   case HM.lookup f opt of
      (Just x)  -> read x
      Nothing   -> def

getFlagString :: Options -> Flag -> String
getFlagString opt f =
   case HM.lookup f opt of
      (Just x)  -> x
      Nothing   -> ""

isDisplayHelp :: FlagStatus -> Bool
isDisplayHelp DisplayHelp = True
isDisplayHelp _           = False

isSyntaxError :: FlagStatus -> Bool
isSyntaxError (SyntaxError _) = True
isSyntaxError _               = False

extractOptions :: FlagStatus -> Options
extractOptions (NoError x) = x

extractError :: FlagStatus -> String
extractError (SyntaxError x) = x
