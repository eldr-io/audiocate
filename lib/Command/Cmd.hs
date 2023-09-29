module Command.Cmd (
 Command (..),
 CommandReturnCode (..),
 interpretCmd,
) where

import Command.EncodeCmd (runEncodeCmd)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Stego.Common (StegoParams(..), EncodingType (LsbEncoding))
import Data.Word (Word64)
import Command.DecodeCmd (runDecodeCmd)

data Command
  = Help
  | Encode String Word64 FilePath FilePath
  | Decode String Word64 FilePath
  deriving (Show)

data CommandReturnCode
  = CmdSuccess
  | CmdFail
  | CmdUnknown
  deriving (Show, Eq)


interpretCmd :: Command -> IO CommandReturnCode
interpretCmd cmd =
  case cmd of
    Help -> do 
      putStrLn "run Help"
      pure CmdSuccess
    (Encode secret timeRange inputFile outputFile) -> do 
      let s = encodeUtf8 (T.pack secret)
      let stegoParams = StegoParams s timeRange 6 LsbEncoding 123
      runEncodeCmd stegoParams inputFile outputFile
      pure CmdSuccess
    (Decode secret timeRange inputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let stegoParams = StegoParams s timeRange 6 LsbEncoding 123
      runDecodeCmd stegoParams inputFile
      pure CmdSuccess


