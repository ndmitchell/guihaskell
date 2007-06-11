
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe
import System.IO

import Control.Concurrent

import Graphics.UI.Gtk.Windows.Dialog
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
import Graphics.UI.Gtk.Glade

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
    compiler <- newVar Hugs
    cHandles <- newVar Nothing

    let f x = getCtrl window x
        dat = Data window
                  (f "txtOut") (f "txtIn") (f "sb")
                  (f "tbRun") (f "tbOpen") (f "tbStop") (f "tbRecent") (f "tbCompiler")
                  running filename tags compiler cHandles

    setupDialog dat
    setupFonts dat
    res <- setupRelations dat

    showWindowMain window
   
   {-
    case res of
        Nothing -> return ()
        Just (pid,inp) -> do
            hPutStrLn inp "\n:quit\n"
            waitForProcess pid
            return ()
  -}

setupDialog :: Data -> IO ()
setupDialog dat@Data{tbRun=tbRun,tbStop=tbStop,txtIn=txtIn,running=running,compiler=compiler} = do
    Just xml <- xmlNew "res/compilerdialog.glade"
    dialog   <- xmlGetWidget xml castToDialog "compilerDialog"
    combo    <- xmlGetWidget xml castToComboBox "compilerSelection"
    response <- dialogRun dialog
    handleResponse combo response
    widgetHide dialog
      where 
	  handleResponse combo response = 
	      case response of
		  ResponseNone   -> setCompiler Nothing compiler
		  ResponseCancel -> setCompiler Nothing compiler
		  ResponseOk     -> flip setCompiler compiler =<< comboBoxGetActiveText combo

setupRelations :: Data -> IO ()
setupRelations dat@Data{tbRun=tbRun,tbStop=tbStop,tbCompiler=tbCompiler,txtIn=txtIn,
    running=running,
    compiler=compiler,
    cHandles=cHandles
    } = do

    startEvaluator dat
    handles <- getVar cHandles
    case handles of
        Nothing -> return ()
        Just (pid,inp) -> do
            tbRun!onClicked += fireCommand dat 
	    tbCompiler!onClicked += (setupDialog dat >> stopEvaluator dat >> startEvaluator dat)
            -- tbStop!onClicked += stopCommand dat pid
            onEnterKey txtIn $ fireCommand dat 
    
    tbRun!enabled =< with1 running not
    tbStop!enabled =<= running
    
    return ()


fireCommand :: Data -> IO ()
fireCommand dat@Data{txtOut=txtOut, txtIn=txtIn,cHandles=cHandles} = do
    handles <- getVar cHandles
    case handles of
	Nothing -> return ()
	Just (pid, inp) -> do
	    s <- getVar (txtIn!text)
	    appendText dat (s ++ "\n")
	    running dat -< True
	    forkIO (hPutStrLn inp s)
	    return ()

{-
stopCommand :: Data -> ProcessHandle -> IO ()
stopCommand dat pid = signalProcess sigINT pid
-}
