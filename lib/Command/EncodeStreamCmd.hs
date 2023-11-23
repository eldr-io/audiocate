module Command.EncodeStreamCmd
  ( runEncodeStreamCmd
  ) where

import Conduit ((.|), iterMC, mapC, runConduitRes)
import Control.Monad.IO.Class (liftIO)
import Data.Audio.Wave (Frame)
import qualified Data.Conduit.Audio as CA
import qualified Data.Conduit.Audio.Sndfile as CA
import Data.Int (Int16)
import Data.Time (getCurrentTime, UTCTime)
import qualified Data.Vector.Storable as VS
import Sound.File.Sndfile (Info(format), getFileInfo)
import Stego.Common (StegoParams, shouldSkipFrame)
import Stego.Encode.Encoder (encodeFrame')

-- | Creates a conduit streaming pipeline that reads 
-- small chunks of raw bytes as Int16 from a source audio file 
-- before transforming them into a list of Frames and encoding them 
-- using the stegoParams. Finally, wraps that runnable conduit
-- inside a new proxy AudioSource and attaches a runner sink on the end 
-- which writes the yielded chunks into the destination outputFile.
runEncodeStreamCmd :: StegoParams -> FilePath -> FilePath -> IO ()
runEncodeStreamCmd stegoP inputFile outputFile = do
  info <- getFileInfo inputFile
  let fmt = format info
  input :: CA.AudioSource m Int16 <- CA.sourceSnd inputFile
  let rate = CA.rate input
  let channels = CA.channels input
  let frames = CA.frames input
  time <- getCurrentTime
  let source =
        CA.source input .| mapC VS.toList .|
        iterMC (liftIO . putStrLn . printChunkReceive . length) .|
        mapC (\x -> [(0, x)] :: [Frame]) .|
        iterMC (liftIO . putStrLn . head . map skipFilterNotify) .|
        mapC (map (doEncodeFrame stegoP time)) .|
        mapC (head . map snd) .|
        iterMC (liftIO . putStrLn . printChunkEncoded . length) .|
        mapC VS.fromList
  runConduitRes $
    CA.sinkSnd outputFile fmt (CA.AudioSource source rate channels frames)

doEncodeFrame :: StegoParams -> UTCTime -> Frame -> Frame
doEncodeFrame stegoP time f = if shouldSkipFrame f then f else encodeFrame' stegoP time f

skipFilterNotify :: Frame -> String
skipFilterNotify f
  | shouldSkipFrame f = "Frame will be skipped by SkipFilter."
  | otherwise = "Frame passed skipfilter and will be encoded."

-- | Helper function for printing the number of samples received in a streaming 
-- chunk for encoding
printChunkReceive :: Int -> String
printChunkReceive size =
  "Received chunk of " ++ show size ++ " samples for encoding..."

-- | Helper function for printing the number of samples successfully encoded.
printChunkEncoded :: Int -> String
printChunkEncoded 0 = "No samples encoded, chunk skipped by SkipFilter."
printChunkEncoded size = "Successfully encoded " ++ show size ++ " samples."
