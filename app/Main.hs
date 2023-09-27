module Main (main) where

import qualified Audiocate (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Audiocate.someFunc

