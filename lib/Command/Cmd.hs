module Command.Cmd
  ( Command(..)
  , CommandReturnCode(..)
  , interpretCmd
  ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)

import Command.DecodeCmd (runDecodeCmd)
import Command.DecodeStreamCmd (runDecodeStreamCmd)
import Command.EncodeCmd (runEncodeCmd)
import Command.EncodeStreamCmd (runEncodeStreamCmd)
import Stego.Common (EncodingType(LsbEncoding), StegoParams(..))
import qualified Stego.Decode.Decoder as DC

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
  | EncodeCmdSuccess DC.DecoderResultList
  | DecodeCmdSuccess DC.DecoderResultList
  | CmdFail String
  | CmdUnknown
  deriving (Eq)

instance Show CommandReturnCode where
  show CmdSuccess = "Command completed successfully."
  show (CmdFail err) = "Command failed with error. " ++ err
  show (EncodeCmdSuccess _) = "Encode command completed successfully."
  show (DecodeCmdSuccess _) = "Decode command completed successfully."
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
      result <- runEncodeCmd stegoParams inputFile outputFile
      case result of
        Left err -> pure (CmdFail err)
        Right res -> pure (EncodeCmdSuccess res)
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
      result <- runDecodeCmd stegoParams inputFile
      case result of
        Left err -> pure (CmdFail err)
        Right res -> pure (DecodeCmdSuccess res)
    (DecodeStream secret timeRange inputFile) -> do
      let s = encodeUtf8 (T.pack secret)
      let t :: Word64 = fromIntegral timeRange
      let stegoParams = StegoParams s t 6 LsbEncoding 0 isRealTime
      runDecodeStreamCmd False stegoParams inputFile
      pure CmdSuccess
