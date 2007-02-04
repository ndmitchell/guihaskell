
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Glib.MainLoop
import System.IO
import System.Process
import GHC.Handle
import GHC.IOBase
import Control.Concurrent.MVar
import Control.Concurrent

main = do
        unsafeInitGUIForThreadedRTS

    
        dialogXmlM <- xmlNew "gtkconsole.glade"
        let dialogXml = case dialogXmlM of
                (Just dialogXml) -> dialogXml
                Nothing -> error "Can't find the glade file \"gtkconsole.glade\""
    
        wnd <- xmlGetWidget dialogXml castToWindow "wnd"
        cmd <- xmlGetWidget dialogXml castToButton "cmd"
        txt <- xmlGetWidget dialogXml castToTextView "txt"
        txtData <- textViewGetBuffer txt

        wnd `onDestroy` mainQuit
        widgetShowAll wnd
        cmd `onClicked` (setupProcess txt >> appendText txt "hello!")
        
        mainGUI


setupProcess txt = do 
        (inp,out,err,pid) <- runInteractiveCommand "ghci"
        putStrLn "Starting interactive command"
        hSetBuffering out NoBuffering
        hSetBuffering err NoBuffering
        hSetBuffering inp NoBuffering
        hSetBinaryMode out True
        hSetBinaryMode err True
        
        
        forkIO (readFrom out inp txt)
        forkIO (readFrom err inp txt)
        return ()



appendText :: TextView -> String -> IO ()
appendText txtOut s = do
    buf <- textViewGetBuffer txtOut
    end <- textBufferGetEndIter buf
    textBufferInsert buf end s


readFrom hndl x txt = do
    c <- hGetChar hndl
    postGUIAsync $ appendText txt [c]
    readFrom hndl x txt
