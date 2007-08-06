

module Util where

import PropLang.Gtk

import Data
import Graphics.UI.Gtk

errorMessage :: Data -> String -> IO ()
errorMessage dat msg = do
    let parent = getWindowRaw (window dat)
    md <- messageDialogNew (Just parent) [] MessageError ButtonsOk msg
    dialogRun md
    widgetHide md
