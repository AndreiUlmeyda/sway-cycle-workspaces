module Mode
  ( parseArgumentsAndProvideHelpText,
    Mode (Next, Previous),
  )
where

import Errors (ErrorMessage)
-- cmd line argument parsing
import Options.Applicative
import Data.Semigroup ((<>))

data Mode = Next | Previous deriving (Eq)

parseArgumentsAndProvideHelpText = execParser argumentParser

argumentParser :: ParserInfo Mode
argumentParser = info modeParserAndHelpText programDescription

modeParserAndHelpText :: Parser Mode
modeParserAndHelpText = modeParser <**> helper

programDescription :: InfoMod Mode
programDescription = fullDesc <> progDesc "Switch to either the next or previous workspace on the focused display if such a thing exists." <> header "sway-cycle-workspaces"

modeParser :: Parser Mode
modeParser = hsubparser (commandNext <> commandPrevious)

commandNext :: Mod CommandFields Mode
commandNext = command "next" parserInfoNext

parserInfoNext :: ParserInfo Mode
parserInfoNext = info parserModeNext infoModNext

parserModeNext :: Parser Mode
parserModeNext = pure Next

infoModNext :: InfoMod Mode
infoModNext = progDesc "Switch to the next workspace"

commandPrevious :: Mod CommandFields Mode
commandPrevious = command "previous" (info (pure Previous) (progDesc "Switch to the previous workspace"))
