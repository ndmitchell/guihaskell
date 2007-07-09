-----------------------------------------------------------------------------
-- 
-- Module      :  Main.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable, uses Gtk2Hs
--
-----------------------------------------------------------------------------

module Main where

import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe
import System.IO

import Control.Concurrent

import Graphics.UI.Gtk hiding (Action, Window, MenuItem, TextView, ToolButton, Event, onClicked)
import Graphics.UI.Gtk.Glade

import System.Exit
import System.Posix.Signals
import System.Process


import Commands
import Data
import Evaluator


main :: IO ()
main = do
    initPropLang
    window <- getWindow "res/guihaskell.glade" "wndMain"
    running <- newVar True
    filename <- newVar Nothing
    tags <- newVar []
    current <- newVar Hugs
    states <- newVar initialStates

    let f x = getCtrl window x
        dat = Data window
                  (f "txtOut") (f "txtIn") (f "txtSelect") (f "sb")
                  (f "tbRun")  (f "tbStop") (f "tbRestart")
		  (f "tbOpen") (f "tbRecent") (f "tbCompiler") (f "tbProfile")
		  (f "miFile") (f "miNew") (f "miQuit")
                  running filename tags current states

    runCompilerDialog >>= switchEvaluator dat
    setupFonts dat
    setupRelations dat

    showWindowMain window

--
-- Run the compiler selection dialog and
-- return the selection
--
runCompilerDialog :: IO (Name)
runCompilerDialog = do
    Just xml <- xmlNew "res/compilerdialog.glade"
    dialog   <- xmlGetWidget xml castToDialog "compilerDialog"
    combo    <- xmlGetWidget xml castToComboBox "compilerSelection"
    response <- dialogRun dialog
    widgetHide dialog
    handleResponse combo response
      where 
	  handleResponse combo response =
	      case response of
		  ResponseOk -> (return . maybe defaultName read) =<< 
				  comboBoxGetActiveText combo
		  _          -> return defaultName
	    where 
	      defaultName = Hugs

--
-- Run an open file dialog and
-- return the selection
--
runFileDialog :: IO (Maybe FilePath)
runFileDialog = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("Okay", ResponseOk), ("Cancel", ResponseCancel)]
    response <- dialogRun dialog
    widgetHide dialog
    handleResponse dialog response
    where
	handleResponse dialog response =
	    case response of
		ResponseOk -> fileChooserGetFilename dialog
		_	   -> return Nothing

--
-- Defines the actions for GUI elements
--
setupRelations :: Data -> IO ()
setupRelations dat@Data
    { tbRun=tbRun, tbStop=tbStop, tbRestart=tbRestart
    , tbOpen=tbOpen, tbCompiler=tbCompiler
    , txtIn=txtIn, txtSelect=txtSelect
    , miQuit=miQuit, miFile=miFile
    , running=running, filename=filename
    } = do

    tbRun!onClicked 	 += fireCommand dat 
    tbRestart!onClicked  += (startWithFile dat)
    tbCompiler!onClicked += (runCompilerDialog >>= switchEvaluator dat)
    tbOpen!onClicked 	 += (runFileDialog >>= setCurrentFile dat >> startWithFile dat)
    -- tbStop!onClicked  += stopCommand dat pid
    onEnterKey txtIn $ fireCommand dat 
   
    -- Menu doesn't work yet
    --miQuit!onActivated += exitWith ExitSuccess
    --miFile!menu =<= foomenu
    
    txtSelect!text =< (with1 filename $ maybe "" id)
    
    tbRun!enabled =< with1 running not
    tbStop!enabled =<= running

--
-- Sends entered text to processes
--
fireCommand :: Data -> IO ()
fireCommand dat@Data{txtOut=txtOut, txtIn=txtIn} = do
    handles <- getHandles dat
    case handles of
	Nothing -> appendText dat "Error: Compiler not running\n"
	Just (inp,_) -> do
	    s <- getVar (txtIn!text)
	    running dat -< True
	    left <- checkCommands dat s
	    case left of 
		Nothing -> return ()
		Just x  -> do
		    appendText dat (s ++ "\n")
		    forkIO (hPutStrLn inp x)
		    return ()

{-
stopCommand :: Data ->  IO ()
stopCommand dat = do 
    handles <- getHandles dat
    case handles of
	Nothing -> return ()
	Just (pid, inp) -> do
	    signalProcess sigINT pid
-}
