
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Glib.MainLoop
import System.IO
import System.Process
import GHC.Handle
import GHC.IOBase
import Control.Concurrent.MVar


main = do
        initGUI
    
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
        cmd `onClicked` setupProcess
        
        mainGUI


setupProcess = do 
        (inp,out,err,pid) <- runInteractiveCommand "ghci"
        putStrLn "Starting interactive command"
        hSetBuffering out NoBuffering
        hSetBuffering err NoBuffering
        hSetBuffering inp NoBuffering
        hSetBinaryMode out True
        hSetBinaryMode err True

        (fd,buf) <- handleToFd out

{-
        c <- readCharFd fd buf
        print c
        
        c <- readCharFd fd buf
        print c
        
        c <- readCharFd fd buf
        print c
-}        
        inputAdd fd [IOIn] 0 (do print "hello" ; c <- readCharFd fd buf; print c; return True)
        return ()


handleToFd :: Handle -> IO (FD, IORef Buffer)
handleToFd (FileHandle a b) = do print "hellow" ; h <- readMVar b ; print "goodbye" ; return (haFD h, haBuffer h)
handleToFd (DuplexHandle a b c) = error $ "DuplexHandle: " ++ a


readCharFd :: FD -> IORef Buffer -> IO Char
readCharFd fd ref = do
    buf <- readIORef ref
    let raw = bufBuf buf
    r <- readRawBuffer "hGetChar" (fromIntegral fd) False raw 0 1
    if r == 0 then ioe_EOF else do
        (c,_) <- readCharFromBuffer raw 0
        return c



{-
-- A normal handle to a file
    FilePath            -- the file (invariant)
    !(MVar Handle__)

  | DuplexHandle            -- A handle to a read/write stream
    FilePath            -- file for a FIFO, otherwise some
                    --   descriptive string.
    !(MVar Handle__)        -- The read side
    !(MVar Handle__)        -- The write side
h = 



withHandle "handleToFd" h $ \ h_ -> do
  -- converting a Handle into an Fd effectively means
  -- letting go of the Handle; it is put into a closed
  -- state as a result. 
  let fd = haFD h_
  flushWriteBufferOnly h_
  unlockFile (fromIntegral fd)
    -- setting the Handle's fd to (-1) as well as its 'type'
    -- to closed, is enough to disable the finalizer that
    -- eventually is run on the Handle.
  return (h_{haFD= (-1),haType=ClosedHandle}, Fd (fromIntegral fd))
-}
