
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe
import System.IO

import Control.Concurrent

import Graphics.UI.Gtk.Windows.Dialog

import System.Posix.Signals
import System.Process


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
    res <- setupRelations dat

    showWindowMain window
    
    case res of
        Nothing -> return ()
        Just (pid,inp) -> do
            hPutStrLn inp "\n:quit\n"
            waitForProcess pid
            return ()


setupRelations dat@Data{tbRun=tbRun,tbStop=tbStop, txtIn=txtIn,
    running=running
    } = do

    proc <- startEvaluator dat
    case proc of
        Nothing -> return ()
        Just (pid,inp) -> do
            tbRun!onClicked += fireCommand dat inp
            -- tbStop!onClicked += stopCommand dat pid
            onEnterKey txtIn $ fireCommand dat inp
    
    tbRun!enabled =< with1 running not
    tbStop!enabled =<= running
    
    return proc
    


fireCommand :: Data -> Handle -> IO ()
fireCommand dat@Data{txtOut=txtOut, txtIn=txtIn} hndl = do
    s <- getVar (txtIn!text)
    appendText dat (s ++ "\n")
    running dat -< True
    forkIO (hPutStrLn hndl s)
    return ()

{-
stopCommand :: Data -> ProcessHandle -> IO ()
stopCommand dat pid = signalProcess sigINT pid
-}
