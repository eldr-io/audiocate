{-# LANGUAGE ImplicitParams #-}
module Main
  ( main
  )
where

import Audiocate (version)
import Control.Monad (void)
import qualified GI.Adw as Adw
import System.Environment (getArgs, getProgName)
import MainWindow (initMainWindow, MainWindow (window))
import AppState (newAppState)

-- | Activates the Adwaita GTK application by initialising 
-- the MainWindow and presenting it.
activate :: Adw.Application -> IO ()
activate app = do
  appState <- newAppState
  mw  <- initMainWindow app appState
  let w = window mw
  w.present

-- | Helper function for toggling the light and dark theme of the 
-- application.
toggleTheme :: Adw.Application -> [String] -> IO ()
toggleTheme _ [] = pure ()
toggleTheme app ["--light"] = do
  sm <- Adw.getApplicationStyleManager app
  Adw.setStyleManagerColorScheme sm Adw.ColorSchemeForceLight
toggleTheme _ _ = pure ()

-- | Main entry point for the GUI application.
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  app <- Adw.new Adw.Application
      [ #applicationId Adw.:= "eldr-io.audiocate.gui",
        Adw.On #activate (activate ?self),
        Adw.On #activate (toggleTheme ?self args)
      ]
  putStrLn $ "Audiocate GUI v" ++ version
  void (app.run $ Just [progName])
