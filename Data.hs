-----------------------------------------------------------------------------
-- 
-- Module      :  Data.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable, uses Gtk2Hs
--
-- Defines the core data structures for GuiHaskell.
--
-- Data passes around some global state. Data includes
-- EvalState, which holds the states of the individual
-- compilers that GuiHaskell can run.
--
-----------------------------------------------------------------------------

module Data (
	Data(..), Evaluator(..), Handles(..),
	empty, getHandles, setHandles, setCurrentFile,
	setupFonts, appendText, appendRed, applyEscape,
	promptCmd
	) where

import PropLang.Gtk
import PropLang.Variable

import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent (ThreadId)
import System.IO (Handle)
import System.Process (ProcessHandle)
import Text.EscapeCodes

import Control.Monad
import Numeric

import Graphics.UI.Gtk hiding (Action, Window, ComboBox, MenuItem, TextView, ToolButton, FontButton, Event, onClicked, onChanged)


data Data = Data {
    -- Main Window and friends
      window :: Window
    , txtOut :: TextView
    , txtIn :: TextView
    , txtSelect :: TextEntry
    , sb :: StatusBar
    
    , tbRun :: ToolButton
    , tbStop :: ToolButton
    , tbRestart :: ToolButton
    , tbOpen :: ToolButton
    , tbRecent :: ToolButton
    , tbProfile :: ToolButton
    , tbPref :: ToolButton

    , cbCompiler :: ComboBox

    , fbFont :: FontButton

    , miFile :: MenuItem
    , miOpen :: MenuItem
    , miQuit :: MenuItem
    , miEdit :: MenuItem
    , miCut :: MenuItem
    , miCopy :: MenuItem
    , miPaste :: MenuItem
    , miTools :: MenuItem
    , miPref :: MenuItem
    , miHelp :: MenuItem
    , miAbout :: MenuItem

    -- Preferences Dialog and friends
    , wndPref :: Window
    , txtExecutable :: TextEntry
    , txtProfCFlags :: TextEntry
    , txtProfRFlags :: TextEntry
    , tbClose :: ToolButton

    -- About dialog
    , wndAbout :: Window

    , running :: Var Bool -- is the code executing
    , filename :: Var (Maybe FilePath) -- the main file loaded
    , outputTags :: Var [String]

    -- Configuration variables
    , profCFlags :: Var String
    , profRFlags :: Var String
    , executable :: Var FilePath

    --
    -- Stores the current evaluator and
    -- the states of background evaluators
    --
    -- When a new evaluator is chosen, the
    -- current evaluator is swapped into the list
    -- and the new evalutor is put into current
    , current :: Var Evaluator
    , states :: Var (Map Evaluator Handles)
    }

--
-- A data structure for storing the compiler-specific
-- details
--
data Handles = Handles {
    handle :: Handle,
    pid :: ProcessHandle,
    outId :: ThreadId,
    errId :: ThreadId
    }

-- hack!
-- shouldn't matter as long as you use Var like an IORef
-- maybe ProcessHandle should instantiate Eq
instance Eq Handles where
    _ == _ = True

data Evaluator = Hugs | GHCi deriving (Show, Read, Eq, Ord)


-- So Main doesn't need to import Map
empty :: Map Evaluator Handles
empty = M.empty

-- Probably belongs in Evaluator.hs
promptCmd :: Evaluator -> String -> String
promptCmd Hugs xs = ":set -p\"" ++ xs ++ "\""
promptCmd GHCi xs = ":set prompt " ++ xs

-- Get the current evaluator
getHandles :: Data -> IO (Maybe Handles)
getHandles dat = do
    c <- getVar $ current dat
    s <- getVar $ states dat
    return $ M.lookup c s

-- Set the handles for the current evaluator
setHandles :: Data -> Maybe Handles -> IO ()
setHandles dat hndls = do
    c <- getVar $ current dat
    s <- getVar $ states dat
    case hndls of
	Nothing -> states dat -< M.delete c s
	Just x  ->
	    case M.lookup c s of
		Nothing -> states dat -< M.insert c x s
		Just x  -> states dat -< M.adjust (\_ -> x) c s

-- Set the currently open file
setCurrentFile :: Data -> Maybe FilePath -> IO ()
setCurrentFile dat path = do
    filename dat -< path

--
--
--
setupFonts :: Data -> IO ()
setupFonts dat@Data{txtOut=out,txtIn=inp} = do
    buf <- textviewBuffer out
    tags <- textBufferGetTagTable buf
    font <- getVar $ (fbFont dat)!text
    
    mapM (addTags tags) [minBound..maxBound]

    fdesc <- fontDescriptionFromString font

    widgetModifyFont (getTextViewRaw out) (Just fdesc)
    widgetModifyFont (getTextViewRaw inp) (Just fdesc)

    where
        addTags tags col = do
            let name = show col
                (r,g,b) = getColor col
                f x = let xs = showHex x "" in ['0' | length xs == 1] ++ xs
                css = "#" ++ f r ++ f g ++ f b
            
            tagFg <- textTagNew (Just $ "fg" ++ name)
            tagBg <- textTagNew (Just $ "bg" ++ name)
            textTagTableAdd tags tagFg
            textTagTableAdd tags tagBg
            set tagFg [textTagForeground := css]
            set tagBg [textTagBackground := css]

--
-- Append text to output area
--
appendText :: Data -> String -> IO ()
appendText dat@Data{txtOut=out} s = do
    buf <- textviewBuffer out
    end <- textBufferGetEndIter buf
    textBufferInsert buf end s
    
    len <- textBufferGetCharCount buf
    strt <- textBufferGetIterAtOffset buf (len - length s)
    end2 <- textBufferGetEndIter buf
    tags <- getVar (outputTags dat)
    mapM_ (f buf strt end2) tags
    where
        f buf strt end tag = textBufferApplyTagByName buf tag strt end


appendRed :: Data -> String -> IO ()
appendRed dat msg = do
    let tags = outputTags dat
    res <- getVar tags
    tags -< ["fgRed"]
    appendText dat msg
    tags -< res


applyEscape :: Data -> EscapeCode -> IO ()
applyEscape dat (FormatAttribute Normal) = outputTags dat -< []
applyEscape dat (FormatForeground Green) = outputTags dat -< ["fgGreen"]
applyEscape _ _ = return ()


--when_ :: Monad m => Bool -> m () -> m ()
--when_ b x = when b (x >> return ())
