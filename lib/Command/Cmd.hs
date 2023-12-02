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
import Command.EncodeStreamCmd (runEncodeStreamCmd)
import Stego.Common (EncodingType(LsbEncoding), StegoParams(..))
import Command.DecodeStreamCmd (runDecodeStreamCmd)

data Command
  = Help
  | Encode String Int FilePath FilePath
  | EncodeStream String Int FilePath FilePath
  | Decode String Int FilePath
  | DecodeStream String Int FilePath

instance Show Command where
  show Help = "HELP"
  show (Encode {}) = "ENCODE"
  show (EncodeStream {}) = "ENCODESTREAM"
  show (Decode {}) = "DECODE"
  show (DecodeStream {}) = "DECODESTREAM"

data CommandReturnCode
  = CmdSuccess
  | CmdFail
  | CmdUnknown
  deriving (Eq)

instance Show CommandReturnCode where
  show CmdSuccess = "Command completed successfully."
  show CmdFail = "Command failed."
  show _ = "Command unknown or fault"

interpretCmd :: Command -> Bool -> IO CommandReturnCode
interpretCmd cmd isRealTime =
  case cmd of
    Help -> do
      putStrLn "run Help"
      pure CmdSuccess
    (Encode secret timeRange inputFile outputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 0 isRealTime
      runEncodeCmd stegoParams inputFile outputFile
      pure CmdSuccess
    (EncodeStream secret timeRange inputFile outputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 0 isRealTime
      runEncodeStreamCmd False stegoParams inputFile outputFile
      pure CmdSuccess
    (Decode secret timeRange inputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 0 isRealTime
      runDecodeCmd stegoParams inputFile
      pure CmdSuccess
    (DecodeStream secret timeRange inputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 0 isRealTime
      runDecodeStreamCmd False stegoParams inputFile
      pure CmdSuccess
