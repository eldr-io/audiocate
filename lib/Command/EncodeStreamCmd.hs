module Command.EncodeStreamCmd
  ( runEncodeStreamCmd
  ) where

import Data.Conduit (runConduitRes, (.|), runConduit)
import qualified Data.Conduit.Audio as CA
import qualified Data.Conduit.Audio.Sndfile as CA
import Data.Int (Int16)
import Sound.File.Sndfile (Info(format), getFileInfo)
import Conduit (mapM_C, runResourceT, mapC, sumC, MonadTrans (lift))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import qualified Data.Vector.Storable as VS
import Stego.Encode.Encoder (encodeFrame')
import Data.Audio.Wave (Frame)

-- runEncodeStreamCmd :: StegoParams -> FilePath -> FilePath -> IO ()
runEncodeStreamCmd stegoP inputFile outputFile = do
  info <- getFileInfo inputFile
  let fmt = format info
  input :: CA.AudioSource m Int16 <- CA.sourceSnd inputFile
  print (CA.frames input)
  print (CA.rate input)
  print (CA.channels input)
  time <- getCurrentTime
  runConduitRes $ CA.source input .| mapC VS.toList .| mapC (\x -> [(0,x)] :: [Frame]) .| mapC (map (encodeFrame' stegoP time)) .| mapC (head . map snd) .| mapC VS.fromList .| mapM_C (liftIO . print)

