module Flags(
   Options,
   Flag(Debug, Help, Filename),
   FlagStatus,
   parseFlags,
   getFlag,
   isSyntaxError,
   isDisplayHelp,
   extractOptions) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable

data Flag = Debug | Help | Filename deriving (Show, Eq)
type Options = HM.HashMap Flag String
data FlagStatus = NoError Options | DisplayHelp | SyntaxError

instance Hashable Flag where
   hashWithSalt a f = hashWithSalt a (show f)

getFlagFromString :: String -> Maybe Flag
getFlagFromString "-d"      = Just Debug
getFlagFromString "--debug" = Just Debug
getFlagFromString "--help"  = Just Help
getFlagFromString "-h"      = Just Help
getFlagFromString "?"       = Just Help
getFlagFromString _         = Nothing

parseFlags :: [String] -> FlagStatus
parseFlags [] = SyntaxError
parseFlags s = parse s HM.empty

parse :: [String] -> Options -> FlagStatus
parse [] opt = NoError opt
parse (s:ss) opt =
   case getFlagFromString s of
      (Just Debug) -> parse ss (HM.insert Debug "True" opt)
      (Just Help) -> DisplayHelp
      Nothing | null ss -> parse ss (HM.insert Filename ('\"' : s ++ "\"") opt)
      Nothing -> SyntaxError

getFlag :: (Read v) => Options -> Flag -> v -> v
getFlag opt f def =
   case HM.lookup f opt of
      Nothing   -> def
      (Just x) -> read x

isDisplayHelp :: FlagStatus -> Bool
isDisplayHelp DisplayHelp = True
isDisplayHelp _           = False

isSyntaxError :: FlagStatus -> Bool
isSyntaxError SyntaxError = True
isSyntaxError _           = False

extractOptions :: FlagStatus -> Options
extractOptions (NoError x) = x
