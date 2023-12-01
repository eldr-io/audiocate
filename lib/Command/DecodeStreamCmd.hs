module Command.DecodeStreamCmd
  ( runDecodeStreamCmd
  ) where

import Conduit ((.|), iterMC, mapC, mapM_C, runConduitRes)
import Control.Monad.IO.Class (liftIO)
import Data.Audio.Wave (Frame)
import qualified Data.Conduit.Audio as CA
import qualified Data.Conduit.Audio.Sndfile as CA
import Data.Int (Int16)
import qualified Data.Vector.Storable as VS
import Data.Word (Word64)
import Stego.Common (StegoParams, TotpPayload, checkTotp, word64ToUtcTime)
import Stego.Decode.Decoder
  ( DecoderResult(DecodedFrameR, SkippedFrame)
  , decodeFrame'
  )

-- | Creates a conduit streaming pipeline that reads 
-- small chunks of raw bytes as Int16 from a source audio file 
-- before transforming them into a list of Frames and decoding them 
-- using the stegoParams. 
-- The sink for this pipeline is printing the decoding result.
runDecodeStreamCmd :: Bool -> StegoParams -> FilePath -> IO ()
runDecodeStreamCmd verbose stegoP inputFile = do
  input :: CA.AudioSource m Int16 <- CA.sourceSnd inputFile
  if verbose then do
    let source =
          CA.source input .| mapC VS.toList .|
          iterMC (liftIO . putStrLn . printChunkReceive . length) .|
          mapC (\x -> [(0, x)] :: [Frame]) .|
          mapC (map (decodeFrame' stegoP)) .|
          mapC (map (toResult stegoP (0, []))) .|
          mapM_C (liftIO . putStrLn . head . map printDecoderResult)
    runConduitRes source
  else do
    let source =
          CA.source input .| mapC VS.toList .|
          mapC (\x -> [(0, x)] :: [Frame]) .|
          mapC (map (decodeFrame' stegoP)) .|
          mapC (map (toResult stegoP (0, []))) .| 
          mapM_C (\_ -> pure ())
    runConduitRes source

-- | Helper function for creating a strongly typed DecoderResult from the 
-- sourced frames passed through the decoder.
toResult ::
     StegoParams
  -> Frame
  -> Maybe (Int, [Int16], (Word64, TotpPayload))
  -> DecoderResult
toResult _ frame Nothing = SkippedFrame frame
toResult stegoP _ (Just (i, samples, (time, payload))) =
  DecodedFrameR
    (i, samples, (time, payload))
    (checkTotp stegoP (word64ToUtcTime time) payload)

-- | Helper function for printing the DecoderResult for each chunk of sourced frames.
printDecoderResult :: DecoderResult -> String
printDecoderResult (SkippedFrame _) =
  "Decoding of payload failed, skipped chunk"
printDecoderResult (DecodedFrameR (_, _, (time, payload)) True) =
  "Successfully verified and decoded chunk. Time: " ++ show (word64ToUtcTime time) ++ ". Payload: " ++ show payload
printDecoderResult (DecodedFrameR (_, _, (_, _)) False) =
  "Failed to verify frame."
printDecoderResult _ = "Invalid DecodeResult"

-- | Helper function for printing the number of received samples in a chunk.
printChunkReceive :: Int -> String
printChunkReceive size =
  "Received chunk of " ++ show size ++ " samples for decoding..."
