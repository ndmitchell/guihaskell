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
	Data (..), EvalState(..), Evaluator(..), Name(..),
	initialStates, getCurrentState, getHandles, setHandles,
	setupFonts, appendText, appendRed, applyEscape
	) where

import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Data.Map (Map)
import qualified Data.Map as M

import System.IO (Handle)
import System.Process (ProcessHandle)
import Text.EscapeCodes

import Control.Monad
import Numeric

import Graphics.UI.Gtk hiding (Action, Window, MenuItem, TextView, ToolButton, Event, onClicked)


data Data = Data {
    window :: Window,
    txtOut :: TextView,
    txtIn :: TextView,
    sb :: StatusBar,
    
    tbRun :: ToolButton,
    tbOpen :: ToolButton,
    tbStop :: ToolButton,
    tbRecent :: ToolButton,
    tbCompiler :: ToolButton,

    running :: Var Bool, -- is the code executing
    filename :: Var (Maybe String), -- the main file loaded
    outputTags :: Var [String],

    eState :: Var EvalState
    }

--
-- Stores the current evaluator and
-- the states of background evaluators
--
-- When a new evaluator is chosen, the
-- current evaluator is swapped into the list
-- and the new evalutor is put into current
--
data EvalState = EvalState {
    current :: Name,
    states :: Map Name Evaluator
    }

instance Eq EvalState where
    x == y = current x == current y -- hackish

--
-- A data structure for storing the compiler-specific
-- details
--
data Evaluator = Evaluator {
    handles :: Maybe EHandle,
    promptCmd :: String -> String
    }

type EHandle = (Handle, Either ProcessHandle FilePath)

data Name = Hugs | GHC | GHCi deriving (Show, Read, Eq, Ord)

--
-- Initialize the evalutor details and states
--
initialStates :: EvalState
initialStates = 
    EvalState {
        current = Hugs,
	states = M.fromList [
	    (Hugs, Evaluator { handles = Nothing, promptCmd = \x -> ":set -p\"" ++ x ++ "\"" }),
	    (GHC, Evaluator { handles = Nothing, promptCmd = \x -> "" }),
	    (GHCi, Evaluator { handles = Nothing, promptCmd = \x -> ":set prompt " ++ x })
	]
    }

--
-- Fetch the current evaluator state
--
getCurrentState :: Data -> IO Evaluator
getCurrentState dat = do
    e <- getVar $ eState dat
    M.lookup (current e) (states e)

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
    e <- getVar $ eState dat
    eState dat -< e { states = M.adjust (\x -> x { handles = h }) (current e) (states e) }

setupFonts :: Data -> IO ()
setupFonts dat@Data{txtOut=txtOut, txtIn=txtIn} = do
    buf <- textviewBuffer txtOut
    tags <- textBufferGetTagTable buf
    
    mapM (addTags tags) [minBound..maxBound]

    fdesc <- fontDescriptionNew
    fontDescriptionSetFamily fdesc "Monospace"

    widgetModifyFont (getTextViewRaw txtOut) (Just fdesc)
    widgetModifyFont (getTextViewRaw txtIn) (Just fdesc)

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


appendText :: Data -> String -> IO ()
appendText dat@Data{txtOut=txtOut} s = do
    buf <- textviewBuffer txtOut
    end <- textBufferGetEndIter buf
    textBufferInsert buf end s
    
    len <- textBufferGetCharCount buf
    strt <- textBufferGetIterAtOffset buf (len - length s)
    end <- textBufferGetEndIter buf
    tags <- getVar (outputTags dat)
    mapM_ (f buf strt end) tags
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
applyEscape dat _ = return ()


when_ :: Monad m => Bool -> m () -> m ()
when_ b x = when b (x >> return ())
