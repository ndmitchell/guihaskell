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

import PropLang.Variable

import Data
import Text.EscapeCodes
import Util


prompt :: String
prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"

--
-- Switch the currently running evaluator
--
switchEvaluator :: Data -> IO ()
switchEvaluator dat = do
    hndl <- getHandles dat
    case hndl of
	Nothing -> do
	    startEvaluator dat Nothing
	Just (Handles inp _ _ _) -> do
	    appendText dat "\nSwitching...\n"
	    putStrLn "Switching evaluators"
	    hPutStrLn inp $ ""

--
-- Start an evaluator
--
startEvaluator :: Data -> Maybe [String] -> IO ()
startEvaluator dat args = do
	name <- getVar $ current dat
	path <- getCompilerPath name
	case path of
	    Nothing -> do
		errorMessage dat $ show name ++ " not found, please install."
	    Just x -> do
		runCompiler x
    where
	runCompiler path = do
	    name <- getVar $ current dat
	    (inp,out,err,pid) <- runExternal path args

	    appendText dat $ "\nLoading " ++ show name ++ "...\n"
	    hPutStrLn inp $ promptCmd name prompt
	    hPutStrLn inp $ "putChar '\\01'"

	    oid <- forkIO (readOut name out)
	    eid <- forkIO (readErr name err)

	    setHandles dat (Just $ Handles inp pid oid eid)

	readOut :: Evaluator -> Handle -> IO ()
	readOut Hugs hndl = do
            c <- hGetContents hndl
            let c2 = filter (/= '\r') $ tail $ dropWhile (/= '\01') c
            mapM_ app $ parseEscapeCodes c2
        readOut GHCi hndl = do
            c <- hGetContents hndl
            let c2 = filter (/= '\r') $ tail $ dropWhile (/= '\01') c
            mapM_ app $ parseEscapeCodes c2

	readErr :: Evaluator -> Handle -> IO ()
	readErr Hugs _ = return ()
        readErr GHCi hndl = do
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
	Just (Handles inp pid _ _) -> do
	    setHandles dat Nothing
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()

--
-- Run a compiler with a file
--
startWithFile :: Data -> IO ()
startWithFile dat = do
    path <- getVar $ filename dat
    case path of
	Nothing -> 
	    errorMessage dat "No file selected for evaluation."
	Just p  -> do
	    stopEvaluator dat
	    appendText dat "Evaluating file...\n"
	    startEvaluator dat $ Just [p]

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

getOtherPath :: Evaluator -> IO (Maybe FilePath)
getOtherPath = findExecutable . map toLower . show
