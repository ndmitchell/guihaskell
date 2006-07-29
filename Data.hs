
module Data where

import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Text.EscapeCodes

import Control.Monad


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

    running :: Var Bool, -- is the code executing
    filename :: Var (Maybe String), -- the main file loaded
    outputTags :: Var [String]
    }


setupFonts :: Data -> IO ()
setupFonts dat@Data{txtOut=txtOut, txtIn=txtIn} = do
    buf <- textviewBuffer txtOut
    tags <- textBufferGetTagTable buf
    
    tagGreen <- textTagNew "fgGreen"
    textTagTableAdd tags tagGreen
    set tagGreen [textTagForeground := "Green"]
    
    fdesc <- fontDescriptionNew
    fontDescriptionSetFamily fdesc "Monospace"

    widgetModifyFont (getTextViewRaw txtOut) (Just fdesc)
    widgetModifyFont (getTextViewRaw txtIn) (Just fdesc)


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


applyEscape :: Data -> EscapeCode -> IO ()
applyEscape dat (FormatAttribute Normal) = outputTags dat -< []
applyEscape dat (FormatForeground Green) = outputTags dat -< ["fgGreen"]
applyEscape dat _ = return ()



when_ b x = when b (x >> return ())
