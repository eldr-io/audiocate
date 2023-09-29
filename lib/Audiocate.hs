module Audiocate (
 run,
 Command(..),
 CommandReturnCode(..),
 ) where

import Command.Cmd (interpretCmd, Command(..), CommandReturnCode(..))

run :: Command -> IO CommandReturnCode
run = interpretCmd
