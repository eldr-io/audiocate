module Command.Cmd
  ( Command(..)
  , CommandReturnCode(..)
  , interpretCmd
  ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)

import Command.DecodeCmd (runDecodeCmd)
import Command.EncodeCmd (runEncodeCmd)
import Stego.Common (EncodingType(LsbEncoding), StegoParams(..))

data Command
  = Help
  | Encode String Int FilePath FilePath
  | Decode String Int FilePath

instance Show Command where
  show Help = "HELP"
  show (Encode {}) = "ENCODE"
  show (Decode {}) = "DECODE"

data CommandReturnCode
  = CmdSuccess
  | CmdFail
  | CmdUnknown
  deriving (Eq)

instance Show CommandReturnCode where
  show CmdSuccess = "Command completed successfully."
  show CmdFail = "Command failed."
  show _ = "Command unknown or fault"

interpretCmd :: Command -> IO CommandReturnCode
interpretCmd cmd =
  case cmd of
    Help -> do
      putStrLn "run Help"
      pure CmdSuccess
    (Encode secret timeRange inputFile outputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 123
      runEncodeCmd stegoParams inputFile outputFile
      pure CmdSuccess
    (Decode secret timeRange inputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 123
      runDecodeCmd stegoParams inputFile
      pure CmdSuccess
