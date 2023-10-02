module View.EncodeView (
  EncodeView (..),
  initEncodeView,
)
where

import Data.GI.Base
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Gtk qualified as Gtk
import qualified GI.Adw as Adw

data EncodeView = EncodeView
  { title :: T.Text
  , id :: T.Text
  , encodeViewBox :: Gtk.Box
  }

initEncodeView :: IO EncodeView
initEncodeView = do
  builder <- Gtk.builderNewFromResource "/gui/View/EncodeView.ui"
  encodeBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- castTo Gtk.Box (fromJust encodeBin)
  pure $ EncodeView "Encode" "encodeView" (fromJust bin)
