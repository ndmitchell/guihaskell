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
import PropLang.Value
import PropLang.Event

import Control.Monad
import Data.Maybe
import System.IO

import Control.Concurrent

import Graphics.UI.Gtk hiding (Action, Window, ComboBox, MenuItem, TextView, ToolButton, FontButton, Event, onClicked, onChanged)
import Graphics.UI.Gtk.Glade

import System.Exit
import System.Posix.Signals
import System.Process

import Config
import Data
import Evaluator
import Prof
import Util

main :: IO ()
main = do
    initPropLang
    confInit
    window <- getWindow "res/guihaskell.glade" "wndMain"
    prefWindow <- getWindow "res/prefdialog.glade" "wndPref"
    aboutWindow <- getWindow "res/about.glade" "wndAbout"
    running <- newVarName "evaluator_on_off" True
    filename <- newVarWithName "selected_filename"
        (newConfValueWithDefault Nothing "selected_filename")
    tags <- newVar []
    history <- newVar ([], [])
    
    -- Configuration variables
    profCFlags <- newVarWithName "profiler_cflags_conf" 
	(newConfValueWithDefault "-prof -auto-all" "profCFlags")
    profRFlags <- newVarWithName "profiler_rflags_conf" 
	(newConfValueWithDefault "+RTS -p" "profRFlags")
    executable <- newVarWithName "executable_name"
	(newConfValueWithDefault "foobar.exe" "executable")
   
   -- Evaluator variables
    current <- newVarWithName "current_evaluator"
	(newConfValueWithDefault Hugs "current_evaluator")
    states <- newVarName "evaluator_states" empty

    let f x = getCtrl window x
	g x = getCtrl prefWindow x
        dat = Data window
                  (f "txtOut") (f "txtIn") (f "txtSelect") (f "sb")
                  (f "tbRun")  (f "tbStop") (f "tbRestart")
		  (f "tbOpen") (f "tbRecent") (f "tbProfile") (f "tbPref")
		  (f "cbCompiler") (f "fbFont")
		  (f "miFile") (f "miOpen") (f "miQuit") 
		  (f "miEdit") (f "miCut") (f "miCopy") (f "miPaste")
		  (f "miTools") (f "miProfile") (f "miPref")
		  (f "midHelp") (f "miAbout")
		  prefWindow
		  (g "txtExecutable") (g "txtProfCFlags") (g "txtProfRFlags")
		  (g "tbClose")
		  aboutWindow
                  running filename tags history
		  profCFlags profRFlags executable font
		  current states

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
    -- This is ugly, but at least it's readable
    { window         =window
    , tbRun          =tbRun
    , tbStop         =tbStop
    , tbRestart      =tbRestart
    , tbOpen         =tbOpen
    , tbPref         =tbPref
    , tbProfile      =tbProfile
    , cbCompiler     =cbCompiler
    , fbFont         =fbFont
    , txtIn          =txtIn 
    , txtSelect      =txtSelect
    , miFile         =miFile
    , miOpen         =miOpen
    , miQuit         =miQuit
    , miProfile      =miProfile
    , miPref         =miPref
    , miHelp         =miHelp
    , miAbout        =miAbout
    , wndPref        =wndPref
    , txtExecutable  =txtExecutable
    , txtProfCFlags  =txtProfCFlags
    , txtProfRFlags  =txtProfRFlags
    , tbClose        =tbClose
    , wndAbout       =wndAbout
    , running        =running
    , filename       =filename
    , executable     =executable
    , profCFlags     =profCFlags
    , profRFlags     =profRFlags
    , current        =current
    } = do

    -- Evaluator events
    tbRun!onClicked 	 += fireCommand dat 
    tbRestart!onClicked  += (startWithFile dat)
    tbOpen!onClicked 	 += (runFileDialog >>= setCurrentFile dat >> startWithFile dat)
    tbStop!onClicked     += stopCommand dat
    onEnterKey txtIn $ fireCommand dat

    -- Evaluator selection
    injectWith (cbCompiler!text) current show
    current =< with1 (cbCompiler!text) (\x -> if null x then Hugs else read x)
    current += switchEvaluator dat

    -- Filename selection 
    injectWith (txtSelect!text) filename (maybe "" id)
    tie (txtSelect!text) filename 
	(\t -> if null t then Nothing else Just t) (maybe "" id)

    -- Evaluator runtime status 
    tbRun!enabled  =<  with1 running not
    tbStop!enabled =<= running

    -- Tools
    tbProfile!onClicked	 += (runProf dat)
    
    -- Config events
    tbPref!onClicked 	 += (showWindow wndPref)
    miPref!onActivated 	 += (showWindow wndPref)
    -- Hack
    tbClose!onClicked	 += (widgetHide $ getWindowRaw wndPref)
    fbFont!text          += setupFonts dat

    -- Need "-<-" first to populate GUI at startup
    -- Then tie them
    txtExecutable!text -<- executable
    txtExecutable!text =<>= executable
    txtProfCFlags!text -<- profCFlags
    txtProfCFlags!text =<>= profCFlags
    txtProfRFlags!text -<- profRFlags
    txtProfRFlags!text =<>= profRFlags
   
    -- Fixed some of the key event bugs. This should work.
    -- The enter key still doesn't work though...
    --enterKey <-  newVarWithName "enter_key_pressed?"
    --               (newPredValue "" ((==) "Return"))
    --enterKey =<= (txtIn!key)
    --enterKey +=  (fireCommand dat)
    upKey    <-  newVarWithName "arrow_up_pressed?"
	            (newPredValue "" ((==) "Up"))
    upKey    =<= (txtIn!key)
    upKey    +=  (nextHistory dat)
    downKey  <-  newVarWithName "arrow_down_pressed?"
	            (newPredValue "" ((==) "Down"))
    downKey  =<= (txtIn!key)
    downKey  +=  (previousHistory dat)

    -- Menus
    miOpen!onActivated    += (raise $ tbOpen!onClicked)
    miQuit!onActivated    += mainQuit
    miProfile!onActivated += (runProf dat)
    miAbout!onActivated   += (showWindow wndAbout)
    
    return ()

--
-- Sends entered text to processes
--
fireCommand :: Data -> IO ()
fireCommand dat@Data{txtOut=txtOut, txtIn=txtIn} = do
    handles <- getHandles dat
    hist <- getVar $ history dat
    case handles of
	Nothing -> errorMessage dat "Can't send command; compiler not running."
	Just (Handles inp _ _ _) -> do
	    s <- getVar (txtIn!text)
	    running dat -< True
	    appendText dat (s ++ "\n")
	    forkIO (hPutStrLn inp s)
	    txtIn!text -< ""
	    if null $ dropWhile isSpace s
	      then history dat -< ([], fst hist ++ snd hist)
	      else history dat -< ([], s : fst hist ++ snd hist)
	
refreshCommand :: Data -> IO ()
refreshCommand dat = do
    path <- getVar $ filename dat
    case path of
	Nothing -> startEvaluator dat Nothing
	Just _  -> startWithFile dat

--
-- Stop the currently running process
--
stopCommand :: Data -> IO ()
stopCommand dat = do
    handles <- getHandles dat
    case handles of
	Nothing -> errorMessage dat "No compiler running to stop."
	Just (Handles inp pid oid eid) -> do
	    killThread oid
	    killThread eid
	    terminateProcess pid
	    waitForProcess pid
	    setHandles dat Nothing
	    startEvaluator dat Nothing

-- Get the next oldest command from history
nextHistory :: Data -> IO ()
nextHistory dat@Data {txtIn=txtIn} = do
    h <- getVar $ history dat
    case h of
	(xs, y:ys) -> do 
	    (txtIn!text) -< y
	    history dat  -< (y:xs, ys)
	_             -> return ()

-- Get a command that's one newer
previousHistory :: Data -> IO ()
previousHistory dat@Data {txtIn=txtIn} = do
    h <- getVar $ history dat
    case h of
	(x:x2:xs, ys) -> do
	    (txtIn!text) -< x2
	    history dat  -< (x2:xs, x:ys)
	_          -> do
	    (txtIn!text) -< ""
	    history dat  -< ([], fst h ++ snd h)

{-
stopCommand :: Data ->  IO ()
stopCommand dat = do 
    handles <- getHandles dat
    case handles of
	Nothing -> return ()
	Just (pid, inp) -> do
	    signalProcess sigINT pid
-}
