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

import Graphics.UI.Gtk hiding (Action, Window, ComboBox, MenuItem, TextView, ToolButton, Event, onClicked, onChanged)
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
    running <- newVarName "evaluator_on_off" True
    filename <- newVarName "filename_selection" Nothing
    tags <- newVar []
    current <- newVarName "current_evaluator" Hugs
    states <- newVar initialStates

    let f x = getCtrl window x
        dat = Data window
                  (f "txtOut") (f "txtIn") (f "txtSelect") (f "sb")
                  (f "tbRun")  (f "tbStop") (f "tbRestart")
		  (f "tbOpen") (f "tbRecent") (f "tbProfile") (f "cbCompiler") 
		  (f "miFile") (f "miNew") (f "miQuit")
                  running filename tags current states

    startEvaluator dat Nothing
    setupFonts dat
    setupRelations dat

    showWindowMain window

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
    , tbOpen=tbOpen, cbCompiler=cbCompiler
    , txtIn=txtIn, txtSelect=txtSelect
    , miQuit=miQuit, miFile=miFile
    , running=running, filename=filename
    , current=current
    } = do

    tbRun!onClicked 	 += fireCommand dat 
    tbRestart!onClicked  += (startWithFile dat)
    tbOpen!onClicked 	 += (runFileDialog >>= setCurrentFile dat >> startWithFile dat)
    -- tbStop!onClicked  += stopCommand dat pid
    onEnterKey txtIn $ fireCommand dat 

    current =< with1 (cbCompiler!text) (\x -> if null x then Hugs else read x)
    current += switchEvaluator dat
   
    -- Menu doesn't work yet
    --miQuit!onActivated += exitWith ExitSuccess
    --miFile!menu =<= foomenu
   
    tie (txtSelect!text) filename (Just . id) (maybe "" id)
    
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
