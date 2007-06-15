
module Data where

import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import System.IO (Handle)
import System.Process
import Text.EscapeCodes

import Control.Monad
import Numeric

import Graphics.UI.Gtk hiding (Action, Window, TextView, ToolButton, Event, onClicked)


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
    current :: Evaluator,
    rest :: [Evaluator]
    }

instance Eq EvalState where
    x == y = current x == current y && rest x == rest y

--
-- A data structure for storing the compiler-specific
-- details
--
data Evaluator = Evaluator {
    name :: Name,
    handles :: Maybe (ProcessHandle, Handle), 
    promptCmd :: String -> String
    }

instance Eq Evaluator where
    x == y = name x == name y -- hack

data Name = Hugs | GHC | GHCi deriving (Show, Read, Eq, Ord)

initialStates :: EvalState
initialStates = 
    EvalState {
	current = Evaluator { name = Hugs, handles = Nothing, promptCmd = \x -> ":set -p\"" ++ x ++ "\"" },
	rest = [
	    Evaluator { name = GHC, handles = Nothing, promptCmd = \x -> "foo" },
	    Evaluator { name = GHCi, handles = Nothing, promptCmd = \x -> ":set prompt " ++ x }
	]
    }

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



when_ b x = when b (x >> return ())
