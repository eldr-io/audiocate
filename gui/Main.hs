{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.GI.Base (AttrOp (On, (:=), (:=>)), new)
import qualified GI.Adw as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getProgName)

import Audiocate (version)

activate :: Adw.Application -> IO ()
activate app = do

  viewStack <- new Adw.ViewStack []
  viewSwitcher <- new Adw.ViewSwitcher [#policy  := Adw.ViewSwitcherPolicyWide ]

  title <-
    new
      Adw.WindowTitle
      [ #title := "Audiocate",
        #subtitle := "Encode and Decode audio"
      ]
  titlebar <- new Adw.HeaderBar [#titleWidget := viewSwitcher]



  window <-
    new
      Adw.ApplicationWindow
      [ #application := app,
        #content := viewStack,
        #defaultWidth := 400
      ]
  window.present

main :: IO ()
main = do
  app <-
    new
      Adw.Application
      [ #applicationId := "eldr-io.audiocate.gui",
        On #activate (activate ?self)
      ]
  putStrLn $ "Audiocate GUI v" ++ version
  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)
