-- | Main entry point for the Audiocate library that encodes and decodes 
-- audio files using a specific payload for verification of audio source.
module Audiocate
  ( run
  , version
  , Command(..)
  , CommandReturnCode(..)
  ) where

import Command.Cmd (Command(..), CommandReturnCode(..), interpretCmd)

-- | Runs the provided Command 
run :: Command -> Bool -> Bool -> IO CommandReturnCode
run cmd isRealTime isVerbose = interpretCmd cmd isRealTime isVerbose

-- | Prints the version string
version :: String
version = "0.2.0.0-alpha"
