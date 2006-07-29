
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe
import System.IO

import Control.Concurrent

import Graphics.UI.Gtk.Windows.Dialog

import System.Posix.Signals


import Data
import Evaluator


main = do
    initPropLang
    window <- getWindow "res/guihaskell.glade" "wndMain"
    running <- newVar True
    filename <- newVar Nothing
    tags <- newVar []
    
    let f x = getCtrl window x
        dat = Data window
                  (f "txtOut") (f "txtIn") (f "sb")
                  (f "tbRun") (f "tbOpen") (f "tbStop") (f "tbRecent")
                  running filename tags

    setupFonts dat
    setupRelations dat

    showWindowMain window


setupRelations dat@Data{tbRun=tbRun,tbStop=tbStop, txtIn=txtIn,
    running=running
    } = do

    proc <- startEvaluator dat
    case proc of
        Nothing -> return ()
        Just (pid,inp) -> do
            tbRun!onClicked += runCommand dat inp
            -- tbStop!onClicked += stopCommand dat pid
            onEnterKey txtIn $ runCommand dat inp
    
    tbRun!enabled =< with1 running not
    tbStop!enabled =<= running
    


runCommand :: Data -> Handle -> IO ()
runCommand dat@Data{txtOut=txtOut, txtIn=txtIn} hndl = do
    s <- getVar (txtIn!text)
    appendText dat (s ++ "\n")
    running dat -< True
    forkIO (hPutStrLn hndl s)
    return ()

{-
stopCommand :: Data -> ProcessHandle -> IO ()
stopCommand dat pid = signalProcess sigINT pid
-}
