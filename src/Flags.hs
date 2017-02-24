--module to handle arguments given to the program.
module Flags(
   Options,
   Flag(Debug, Help, Filename {- , Input, Width, Height -} ),
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

{- REPRESENTATION CONVENTION: Each constructor represents a flag with the same name.
   REPRESENTATION INVARIANT: True
-}
data Flag = Debug | Help | Filename {-| Input | Width | Height-} deriving (Show, Eq)

{- REPRESENTATION CONVENTION:
      HashMap f s maps a Flag f to a String s that holds some kind of information
      about the Flag f. If a Flag f is not mapped to a String means that f isn't set
      to anything.
   REPRESENTATION INVARIANT: True

-}
type Options = HM.HashMap Flag String

{- REPRESENTATION CONVENTION:
      Holds the result of the flag parsing.
      - NoError o means that no error occured and o is an Options holding valid flags.
      - DisplayHelp means that a help screen should be printed.
      - SyntaxError e means that some kind of error occured with the parsing.
         e is a String that holds the error message.

   REPRESENTATION INVARIANT:
      NoError o, o must be a valid Options.
      SyntaxError e, e must be an actual error message.
-}
data FlagStatus = NoError Options | DisplayHelp | SyntaxError String

instance Hashable Flag where
   hashWithSalt a f = hashWithSalt a (show f)

{- getFlagFromString s
   PRE: True
   POST: (Just f) if s is a string representation to the Flag f
         Nothing is s doesn't match anything.
   EXAMPLES:
      getFlagFromString "asdasd" = Nothing
      getFlagFromString "-d" = Just Debug
-}
getFlagFromString :: String -> Maybe Flag
getFlagFromString "-d"       = Just Debug
getFlagFromString "--debug"  = Just Debug
getFlagFromString "--help"   = Just Help
getFlagFromString "-h"       = Just Help
getFlagFromString "?"        = Just Help
getFlagFromString _          = Nothing

{- parseFlags s
   PRE: True
   POST: NoError o' if every string in s parsed to a valid flag. o' is an Options with the flags stored in it.
         DisplayHelp if any string represented the Help-flag
         SyntaxError e if any error occured. e is the error message.
-}
parseFlags :: [String] -> FlagStatus
parseFlags [] = SyntaxError "no flags found"
parseFlags s = parse s HM.empty
   where
      --helper function that parses s string by string
      parse :: [String] -> Options -> FlagStatus
      parse [] opt = NoError opt
      parse (s:ss) opt =
         case getFlagFromString s of
            (Just Debug)      -> parse ss (HM.insert Debug "True" opt)
            (Just Help)       -> DisplayHelp
            Nothing | null ss -> parse ss (HM.insert Filename s opt)
            Nothing           -> SyntaxError ("'"++ s ++ "'" ++ " is not a valid flag")
            _                 -> error "something wierd happened in parse"


{- getFlagString o f d
   PRE: True
   POST: the value associated with f in o parsed with the read function to be the
         same type as d. If f doesn't exist in o, d is returned instead.
   EXAMPLES:
      getFlag (Options with Debug mapped to "True") Debug False = True
      getFlag (Options with Debug mapped to "True") Help False = False
-}
getFlag :: (Read v) => Options -> Flag -> v -> v
getFlag opt f def =
   case HM.lookup f opt of
      (Just x)  -> read x
      Nothing   -> def

{- getFlagString o f
   PRE: True
   POST: the string value associated with f in o. If f doesn't exist in o, an
         empty string is returned instead.
-}
getFlagString :: Options -> Flag -> String
getFlagString opt f =
   case HM.lookup f opt of
      (Just x)  -> x
      Nothing   -> ""

{- isDisplayHelp f
   PRE: True
   POST: true if f is DisplayHelp
-}
isDisplayHelp :: FlagStatus -> Bool
isDisplayHelp DisplayHelp = True
isDisplayHelp _           = False

{- isSyntaxError f
   PRE: True
   POST: true if f is SyntaxError
-}
isSyntaxError :: FlagStatus -> Bool
isSyntaxError (SyntaxError _) = True
isSyntaxError _               = False

{- extractOptions f
   PRE: f is NoError
   POST: the Options stored in f
-}
extractOptions :: FlagStatus -> Options
extractOptions (NoError x) = x
extractOptions _           = error "extractOptions only works with NoError constructor"

{- extractError f
   PRE: f is SyntaxError
   POST: the error message stored in f
-}
extractError :: FlagStatus -> String
extractError (SyntaxError x) = x
extractError _           = error "extractOptions only works with SyntaxError constructor"
