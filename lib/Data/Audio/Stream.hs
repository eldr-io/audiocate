module Data.Audio.Stream
  ( sourceSnd
  , sinkSnd
  ) where

import Control.Monad (void)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import Data.Conduit.Audio
import qualified Data.Conduit.List as CL
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndBuf

-- | Uses @libsndfile@ to load an audio file as a stream.
sourceSnd :: (MonadResource m, Snd.Sample a) => FilePath -> IO (AudioSource m a, Snd.Format)
sourceSnd fp = sourceSndWithHandle fp $ \_ -> return ()

-- | Lets you perform arbitrary setup on the @libsndfile@ handle before decoding.
sourceSndWithHandle ::
     (MonadResource m, Snd.Sample a)
  => FilePath
  -> (Snd.Handle -> m ())
  -- ^ Perform any setup necessary with the @libsndfile@ handle
  -> IO (AudioSource m a, Snd.Format)
sourceSndWithHandle fp setup
  -- TODO: allow user to supply Snd.Format for raw files?
 = do
  info <- Snd.getFileInfo fp
  let r = fromIntegral $ Snd.samplerate info
      c = Snd.channels info
      src =
        C.bracketP (Snd.openFile fp Snd.ReadMode Snd.defaultInfo) Snd.hClose $ \h -> do
          lift $ setup h
          fix $ \loop ->
            liftIO (Snd.hGetBuffer h chunkSize) >>= \mx ->
              case mx of
                Nothing -> return ()
                Just buf -> do
                  C.yield $ SndBuf.fromBuffer buf
                  loop
  return $ (AudioSource src r c $ Snd.frames info, Snd.format info)

-- | Uses @libsndfile@ to write an audio stream to a file.
sinkSnd ::
     (MonadResource m, Snd.Sample a)
  => FilePath
  -> Snd.Format
  -> AudioSource m a
  -> m ()
sinkSnd fp fmt = do
  sinkSndWithHandle fp fmt $ \_ -> return ()

-- | Lets you perform arbitrary setup on the @libsndfile@ handle before encoding.
sinkSndWithHandle ::
     (MonadResource m, Snd.Sample a)
  => FilePath
  -> Snd.Format
  -> (Snd.Handle -> m ())
  -> AudioSource m a
  -> m ()
sinkSndWithHandle fp fmt setup (AudioSource s r c _) =
  C.runConduit $
  (s .|) $ do
    C.bracketP
      (Snd.openFile fp Snd.WriteMode $
       Snd.defaultInfo
         {Snd.format = fmt, Snd.samplerate = round r, Snd.channels = c})
      Snd.hClose $ \h -> do
      lift $ setup h
      CL.mapM_ $ liftIO . void . Snd.hPutBuffer h . SndBuf.toBuffer
