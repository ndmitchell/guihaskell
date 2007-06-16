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
	evalFile
	) where

import Data

import Data.Char

import System.IO
import System.Process
import System.Directory
import Control.Concurrent
import PropLang.Gtk
import PropLang.Variable

import Text.EscapeCodes


prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"

--
-- Switch the currently running evaluator
--
switchEvaluator :: Data -> Name -> IO ()
switchEvaluator dat n = do
    setCurrent dat n
    pair <- getHandles dat
    case pair of
	Nothing -> do
	    startEvaluator dat n Nothing
	Just (pid,inp) -> do
	    appendText dat "\nSwitching...\n"
	    putStrLn "Switching evaluators"
	    hPutStrLn inp $ ""
    where
	setCurrent :: Data -> Name -> IO ()
	setCurrent dat@Data{eState=eState} n = do
	    e <- getVar eState
	    eState -< e { current = n }

--
-- Start an evaluator
--
startEvaluator :: Data -> Name -> Maybe [String] -> IO ()
startEvaluator dat@Data{txtOut=txtOut} name args = do
	s <- getCurrentState dat
	path <- getCompilerPath name
        case path of
            Nothing -> do
                appendText dat "Compiler not found, please install"
            Just x -> do
                (inp,out,err,pid) <- runInteractiveProcess x (maybe [] id args) Nothing Nothing
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
		setHandles dat $ Just (pid,inp)
    where
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
	Just (pid,inp) -> do
	    setHandles dat Nothing
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()

--
-- Run a compiler with a file
--
evalFile :: Data -> Maybe FilePath -> IO ()
evalFile dat@Data{eState=eState} path = do
    e <- getVar eState
    case path of
	Nothing -> 
	    return ()
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
