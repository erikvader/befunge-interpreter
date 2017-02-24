module Flags(
   Options,
   Flag(Debug, Help, Filename, Input),
   FlagStatus,
   parseFlags,
   getFlag,
   getFlagString,
   isSyntaxError,
   isDisplayHelp,
   extractOptions) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable

data Flag = Debug | Help | Filename | Input deriving (Show, Eq)
type Options = HM.HashMap Flag String
data FlagStatus a = NoError a | DisplayHelp | SyntaxError

instance Hashable Flag where
   hashWithSalt a f = hashWithSalt a (show f)

getFlagFromString :: String -> Maybe Flag
getFlagFromString "-d"      = Just Debug
getFlagFromString "--debug" = Just Debug
getFlagFromString "--help"  = Just Help
getFlagFromString "-h"      = Just Help
getFlagFromString "?"       = Just Help
getFlagFromString "-i"      = Just Input
getFlagFromString "--input" = Just Input
getFlagFromString _         = Nothing

parseFlags :: [String] -> FlagStatus Options
parseFlags [] = SyntaxError
parseFlags s = parse s HM.empty

parse :: [String] -> Options -> FlagStatus Options
parse [] opt = NoError opt
parse (s:ss) opt =
   case getFlagFromString s of
      (Just Debug)      -> parse ss (HM.insert Debug "True" opt)
      (Just Help)       -> DisplayHelp
      (Just Input)      -> case parseInput ss of
                              (NoError (sss, v)) -> parse sss (HM.insert Input v opt)
                              a                  -> SyntaxError
      Nothing | null ss -> parse ss (HM.insert Filename s opt)
      Nothing           -> SyntaxError

parseInput :: [String] -> FlagStatus ([String], String)
parseInput []     = SyntaxError
parseInput (s:ss) = NoError (ss, s)

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

isDisplayHelp :: FlagStatus a -> Bool
isDisplayHelp DisplayHelp = True
isDisplayHelp _           = False

isSyntaxError :: FlagStatus a -> Bool
isSyntaxError SyntaxError = True
isSyntaxError _           = False

extractOptions :: FlagStatus a -> a
extractOptions (NoError x) = x
