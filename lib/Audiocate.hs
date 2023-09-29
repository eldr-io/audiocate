module Audiocate (
 run,
 version,
 Command(..),
 CommandReturnCode(..),
 ) where

import Command.Cmd (interpretCmd, Command(..), CommandReturnCode(..))

run :: Command -> IO CommandReturnCode
run = interpretCmd

version :: String
version = "0.1.0.0-alpha"
