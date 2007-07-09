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
	startWithFile, runExternal
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
    current dat -< n
    hndls <- getHandles dat
    case hndls of
	Nothing -> do
	    startEvaluator dat n Nothing
	Just (inp,hndl) -> do
	    appendText dat "\nSwitching...\n"
	    putStrLn "Switching evaluators"
	    case hndl of
	      Left _     -> hPutStrLn inp $ ""
	      Right file -> do removeFile file; putStrLn "Removed temp buffer"

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
		    Just _ -> do
			runCompiler x
		    Nothing -> do
			case name of
			    Hugs -> do
				runCompiler x
			    GHCi -> do
				runCompiler x
			    GHC -> do
				(file, hndl) <- openTempFile "/tmp" "guihaskell.hs"
				hSetBuffering hndl NoBuffering
				setHandles dat $ Just (hndl,Right file)
    where
	runCompiler path = do
	    s <- getCurrentState dat
	    (inp,out,err,pid) <- runExternal path args

	    appendText dat $ "\nLoading " ++ show name ++ "...\n"
	    hPutStrLn inp $ (promptCmd s) prompt
	    hPutStrLn inp $ "putChar '\\01'"

	    forkIO (readOut out)
	    forkIO (readErr err)
	    setHandles dat $ Just (inp,Left pid)

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
		_    -> getOtherPath c

--
-- Useful wrapper around runInteractiveProcess
--
runExternal :: FilePath -> Maybe [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
      hndls@(inp, out, err, _) <- runInteractiveProcess path
				    (maybe [] id args)
				    Nothing Nothing
      putStrLn $ "Starting external tool: " ++ path
      hSetBuffering out NoBuffering
      hSetBuffering err NoBuffering
      hSetBuffering inp NoBuffering
      hSetBinaryMode out True
      hSetBinaryMode err True
      return hndls

--
-- Stop the currently running evaluator
--
stopEvaluator :: Data -> IO ()
stopEvaluator dat = do
    hndls <- getHandles dat
    case hndls of
	Nothing -> return ()
	Just (inp,Left pid) -> do
	    setHandles dat Nothing
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()
	Just (_,Right file) -> do
	    setHandles dat Nothing
	    removeFile file

--
-- Run a compiler with a file
--
startWithFile :: Data -> IO ()
startWithFile dat = do
    comp <- getVar $ current dat
    path <- getVar $ filename dat
    case path of
	Nothing -> 
	    appendText dat "Error: No file selected.\n"
	Just p  -> do
	    stopEvaluator dat
	    appendText dat "Evaluating file...\n"
	    startEvaluator dat comp $ Just [p]

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

getOtherPath :: Name -> IO (Maybe FilePath)
getOtherPath = findExecutable . map toLower . show
