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
	Data (..), Evaluator(..), Name(..),
	initialStates, getCurrentState, getHandles, setHandles, setCurrentFile,
	setupFonts, appendText, appendRed, applyEscape
	) where

import PropLang.Gtk
import PropLang.Variable

import Data.Map (Map)
import qualified Data.Map as M

import System.IO (Handle)
import System.Process (ProcessHandle)
import Text.EscapeCodes

import Control.Monad
import Numeric

import Graphics.UI.Gtk hiding (Action, Window, ComboBox, MenuItem, TextView, ToolButton, Event, onClicked, onChanged)


data Data = Data {
      window :: Window
    , txtOut :: TextView
    , txtIn :: TextView
    , txtSelect :: TextEntry
    , txtFlags :: TextEntry
    , sb :: StatusBar
    
    , tbRun :: ToolButton
    , tbStop :: ToolButton
    , tbRestart :: ToolButton
    , tbOpen :: ToolButton
    , tbRecent :: ToolButton
    , tbProfile :: ToolButton

    , cbCompiler :: ComboBox

    , miFile :: MenuItem
    , miNew :: MenuItem
    , miQuit :: MenuItem

    , running :: Var Bool -- is the code executing
    , filename :: Var (Maybe FilePath) -- the main file loaded
    , outputTags :: Var [String]

    -- 
    -- Configuration variables
    --

    , profFlags :: Var String

    --
    -- Stores the current evaluator and
    -- the states of background evaluators
    --
    -- When a new evaluator is chosen, the
    -- current evaluator is swapped into the list
    -- and the new evalutor is put into current
    , current :: Var Name
    , states :: Var (Map Name Evaluator)
    }

--
-- A data structure for storing the compiler-specific
-- details
--
data Evaluator = Evaluator {
    handles :: Maybe EHandle,
    promptCmd :: String -> String
    }

-- hack!
instance Eq Evaluator where
    _ == _ = True

type EHandle = (Handle, Either ProcessHandle FilePath)

data Name = Hugs | GHC | GHCi deriving (Show, Read, Eq, Ord)

--
-- Initialize the evalutor details and states
--
initialStates :: Map Name Evaluator
initialStates = 
	M.fromList [
	    (Hugs, Evaluator { handles = Nothing, promptCmd = \x -> ":set -p\"" ++ x ++ "\"" }),
	    (GHC, Evaluator { handles = Nothing, promptCmd = \_ -> "" }),
	    (GHCi, Evaluator { handles = Nothing, promptCmd = \x -> ":set prompt " ++ x })
	]

--
-- Fetch the current evaluator state
--
getCurrentState :: Data -> IO Evaluator
getCurrentState dat = do
    c <- getVar $ current dat
    s <- getVar $ states dat
    M.lookup c s

--
-- Get the handles for the current evaluator
--
getHandles :: Data -> IO (Maybe EHandle)
getHandles dat = do
    s <- getCurrentState dat
    return $ handles s

--
-- Set the handles for the current evaluator
--
setHandles :: Data -> Maybe EHandle -> IO ()
setHandles dat h = do
    c <- getVar $ current dat
    s <- getVar $ states dat
    states dat -< M.adjust (\x -> x { handles = h }) c s

--
-- Set the currently open file
--
setCurrentFile :: Data -> Maybe FilePath -> IO ()
setCurrentFile dat path = do
    filename dat -< path

--
--
--
setupFonts :: Data -> IO ()
setupFonts Data{txtOut=out,txtIn=inp} = do
    buf <- textviewBuffer out
    tags <- textBufferGetTagTable buf
    
    mapM (addTags tags) [minBound..maxBound]

    fdesc <- fontDescriptionNew
    fontDescriptionSetFamily fdesc "Monospace"

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
