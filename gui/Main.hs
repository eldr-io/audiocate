{-# LANGUAGE ImplicitParams #-}
module Main
  ( main
  )
where

import Audiocate (version)
import Control.Monad (void)
import GI.Adw qualified as Adw
import System.Environment (getArgs, getProgName)
import MainWindow (initMainWindow, MainWindow (window))
import Data.Audio.Wave (WaveAudio)
import Control.Concurrent (newEmptyMVar, MVar)
import AppState (AppState (..), newAppState)


activate :: Adw.Application -> IO ()
activate app = do
  appState <- newAppState
  mw  <- initMainWindow app appState
  let w = window mw
  w.present

main :: IO ()
main = do
  app <- Adw.new Adw.Application
      [ #applicationId Adw.:= "eldr-io.audiocate.gui",
        Adw.On #activate (activate ?self)
      ]
  sm <- Adw.getApplicationStyleManager app
  Adw.setStyleManagerColorScheme sm Adw.ColorSchemeForceLight
  putStrLn $ "Audiocate GUI v" ++ version
  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)
