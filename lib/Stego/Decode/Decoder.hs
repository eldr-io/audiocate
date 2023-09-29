-- |
-- - Decoding processor which allows for the enqueueing of Frames
-- - to be decoded and runs a dedicated thread for dequeueing and decoding frames
module Stego.Decode.Decoder
  ( newDecoder,
    getResultChannel,
    enqueueFrame,
    stopDecoder,
    DecoderResult (..),
    mapDecoderOpQToResultChan,
    DecoderResultList,
    getResultStats,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TChan,
    TMVar,
    TQueue,
    dupTChan,
    newBroadcastTChanIO,
    newEmptyTMVarIO,
    newTQueueIO,
    putTMVar,
    readTChan,
    readTQueue,
    writeTChan,
    writeTQueue,
  )
import Control.Monad (void)
import Control.Monad.STM (atomically)
import Data.Audio.Wave (Frame)
import Data.List (foldl')
import Stego.Common
  ( DecodedFrame,
    EncodingType (..),
    StegoParams (..),
    checkTotp,
    getEncodingType,
    readBinWord32,
    readBinWord64,
    word64ToUtcTime, shouldSkipFrame,
  )
import Stego.Decode.LSB qualified
import Stego.Encode.Encoder qualified as EC
import Text.Printf (printf)

data DecoderOp = DecodeFrame Frame | StopDecoder (TMVar ())

type DecoderOpQ = TQueue DecoderOp

data DecoderResult
  = DecodedFrameR DecodedFrame Bool
  | SkippedFrame Frame
  | StoppingDecoder

type DecoderResultList = [DecoderResult]

data DecoderResultStats = DRS !Int !Int !Int !Int

instance Show DecoderResultStats where
  show (DRS t v f s) =
    "=========================\n Total Frames : "
      <> printf "%d\n Skipped      : %d\n Unverified   : %d\n Verified     : %d (%.2f" t s f v score
      <> "%)\n=========================\n"
    where
      score :: Double = if v == 0 then 0.0 else 100.0 * fromIntegral v / fromIntegral (t - s)

data Decoder = Decoder
  { stegoParams :: StegoParams,
    opQ :: DecoderOpQ,
    resultChan :: TChan DecoderResult
  }

getResultStats :: DecoderResultList -> DecoderResultStats
getResultStats dcl = countStatsDcl' dcl (DRS 0 0 0 0)

-- | Used to traverse a DecoderResultList only once and accumulate all interesting
-- count statistics, returning the DecoderResultStats
countStatsDcl' :: DecoderResultList -> DecoderResultStats -> DecoderResultStats
countStatsDcl' = flip (foldl' go)
  where
    go (DRS p q r s) f = case f of
      DecodedFrameR _ True ->
        DRS (p + 1) (q + 1) r s
      DecodedFrameR _ False ->
        DRS (p + 1) q (r + 1) s
      SkippedFrame _ ->
        DRS (p + 1) q r (s + 1)
      _ ->
        DRS (p + 1) q r s

-- | Creates a new decoder that can decode frames using
-- the provided StegoParams
newDecoder :: StegoParams -> IO Decoder
newDecoder stegoParams = do
  opQ <- newTQueueIO
  resultChan <- newBroadcastTChanIO
  let dec = Decoder stegoParams opQ resultChan
  void $ forkIO $ runDecoder dec
  return dec

getResultChannel :: Decoder -> IO (TChan DecoderResult)
getResultChannel = atomically . dupTChan . resultChan

enqueueFrame :: Decoder -> Frame -> IO ()
enqueueFrame dec f = atomically $ writeTQueue (opQ dec) (DecodeFrame f)

-- | Sets the stop signal TMVar for the decoder to break out
-- it's run loop.
stopDecoder :: Decoder -> IO (TMVar ())
stopDecoder dec = do
  m <- newEmptyTMVarIO
  atomically $ writeTQueue (opQ dec) (StopDecoder m)
  pure m

-- | Creates a mapping thread that forwards Ops coming from an
-- EncoderResult channel into Decoder Ops. Used for sequencing so that
-- encoder results can immedeatily be handled by a decoder.
mapDecoderOpQToResultChan :: Decoder -> TChan EC.EncoderResult -> IO ()
mapDecoderOpQToResultChan dec channel = void $ forkIO loop
  where
    loop = do
      res <- atomically $ readTChan channel
      case res of
        (EC.EncodedFrame f) -> do
          enqueueFrame dec f
          loop
        EC.StoppingEncoder -> do
          _ <- stopDecoder dec
          putStrLn "Received stopping encoder signal on chan"
        (EC.SkippedFrame f) -> do
          enqueueFrame dec f
          loop

decodeFrame' :: StegoParams -> Frame -> Maybe DecodedFrame
decodeFrame' stegoParams frame
  | length frame < 128 = Nothing
  | getEncodingType stegoParams == LsbEncoding =
      Just (frame, (time64, totp32))
  | otherwise = Nothing
  where
    (time, totp) = Stego.Decode.LSB.decodeFrame frame
    time64 = readBinWord64 time
    totp32 = readBinWord32 totp

runDecoder :: Decoder -> IO ()
runDecoder dec = loop
  where
    toResult frame Nothing = SkippedFrame frame
    toResult _ (Just (frame, (time, payload))) =
      DecodedFrameR
        (frame, (time, payload))
        (checkTotp (stegoParams dec) (word64ToUtcTime time) payload)
    loop = do
      op <- atomically $ readTQueue (opQ dec)
      case op of
        (DecodeFrame f) -> do
          let shouldSkip = shouldSkipFrame f
          if shouldSkip
            then do
              atomically $ writeTChan (resultChan dec) (SkippedFrame f)
              loop
            else do
              let decoded = decodeFrame' (stegoParams dec) f
              atomically $ writeTChan (resultChan dec) (toResult f decoded)
              loop
        (StopDecoder m) -> atomically $ do
          writeTChan (resultChan dec) StoppingDecoder
          putTMVar m ()
