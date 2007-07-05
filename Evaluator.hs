-----------------------------------------------------------------------------
-- 
-- Module      :  Evaluator.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable, uses Gtk2Hs
--
-- Evaluator functions.
--
-----------------------------------------------------------------------------

module Evaluator (
	switchEvaluator, startEvaluator, stopEvaluator,
	startWithFile
	) where

import Control.Concurrent
import Data.Char
import System.Directory
import System.IO
import System.Process

import Data
import PropLang.Variable
import Text.EscapeCodes


prompt :: String
prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"

--
-- Switch the currently running evaluator
--
switchEvaluator :: Data -> Name -> IO ()
switchEvaluator dat n = do
    setCurrent
    handles <- getHandles dat
    case handles of
	Nothing -> do
	    startEvaluator dat n Nothing
	Just (inp,_,file) -> do
	    appendText dat "\nSwitching...\n"
	    putStrLn "Switching evaluators"
	    case file of
	      Just x   -> do removeFile x; putStrLn "Removed temp buffer"
	      Nothing  -> hPutStrLn inp $ ""
    where
	setCurrent :: IO ()
	setCurrent = do
	    e <- getVar $ eState dat
	    eState dat -< e { current = n }

--
-- Start an evaluator
--
startEvaluator :: Data -> Name -> Maybe [String] -> IO ()
startEvaluator dat name args = do
	path <- getCompilerPath name
	case path of
	    Nothing -> do
		appendText dat "Compiler not found, please install"
	    Just x -> do
		case args of
		    Just as -> do
			runSeparate x
		    Nothing -> do
			case name of
			    Hugs -> do
				runSeparate x
			    GHCi -> do
				runSeparate x
			    GHC -> do
				(file, hndl) <- openTempFile "/tmp" "guihaskell.hs"
				hSetBuffering hndl NoBuffering
				setHandles dat $ Just (hndl,Nothing,Just file)
			    _   -> 
				error "This shouldn't happen"
	      
    where
	runSeparate path = do
	    s <- getCurrentState dat
	    (inp,out,err,pid) <- runInteractiveProcess path
				  (maybe [] id args) 
				  Nothing Nothing
	    putStrLn "Starting interactive command"
	    hSetBuffering out NoBuffering
	    hSetBuffering err NoBuffering
	    hSetBuffering inp NoBuffering
	    hSetBinaryMode out True
	    hSetBinaryMode err True

	    appendText dat $ "\nLoading " ++ show name ++ "...\n"
	    hPutStrLn inp $ (promptCmd s) prompt
	    hPutStrLn inp $ "putChar '\\01'"

	    forkIO (readOut out)
	    forkIO (readErr err)
	    setHandles dat $ Just (inp,Just pid,Nothing)

        readOut hndl = do
            c <- hGetContents hndl
            let c2 = filter (/= '\r') $ tail $ dropWhile (/= '\01') c
            mapM_ app $ parseEscapeCodes c2

        readErr hndl = do
            c <- hGetContents hndl
            let c2 = filter (/= '\r') c
            mapM_ (\x -> appendRed dat [x]) c2

        app (Left c) = appendText dat [c]
        app (Right (FormatUnknown 50)) = running dat -< False
        app (Right e) = applyEscape dat e

	getCompilerPath c = do
	    case c of
		Hugs -> getHugsPath
		--"GHCI" -> getGHCiPath
		_      -> getOtherPath c

--
-- Stop the currently running evaluator
--
-- Should this stop all evaluators?
--
stopEvaluator :: Data -> IO ()
stopEvaluator dat = do
    handles <- getHandles dat
    case handles of
	Nothing -> return ()
	Just (inp,Just pid,_) -> do
	    setHandles dat Nothing
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()
	Just (inp,_,Just file) -> do
	    setHandles dat Nothing
	    removeFile file

--
-- Run a compiler with a file
--
startWithFile :: Data -> Maybe FilePath -> IO ()
startWithFile dat path = do
    e <- getVar $ eState dat
    case path of
	Nothing -> 
	    appendText dat "Error: No file selected.\n"
	Just p  -> do
	    stopEvaluator dat
	    startEvaluator dat (current e) $ Just [p]

getHugsPath :: IO (Maybe FilePath)
getHugsPath = do
    path <- findExecutable "hugs"
    case path of
        Just x -> return $ Just x
        Nothing -> do
            let guesses = ["c:\\Program Files\\WinHugs\\hugs.exe"]
            res <- mapM doesFileExist guesses
            let ans = map snd $ filter fst $ zip res guesses
            return $ if null ans then Nothing else Just (head ans)

getGHCiPath :: IO (Maybe FilePath)
getGHCiPath = findExecutable "ghci"

getOtherPath :: Name -> IO (Maybe FilePath)
getOtherPath = findExecutable . map toLower . show
