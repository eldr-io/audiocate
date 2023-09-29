module Main (main) where

import Audiocate (run, Command(..), CommandReturnCode (..))

main :: IO ()
main = do
  rc <- run (Encode "hello123" 5 "test/corpus/sample1.wav" "test/output/sample1_cmd.wav")
  if rc == CmdSuccess then do
    rc2 <- run (Decode "hello123" 5 "test/output/sample1_cmd.wav")
    print rc2
  else
    print rc

