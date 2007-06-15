
module Evaluator where


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
	    startEvaluator dat n
	Just (pid,inp) -> do
	    appendText dat "\nSwitching...\n"
	    hPutStrLn inp $ ""
	    return ()
    where
	setCurrent :: Data -> Name -> IO ()
	setCurrent dat@Data{eState=eState} n = do
	    e <- getVar eState
	    if (name $ current e) == n  --don't try to change if we're not changing
	      then return ()
	      else do
		  eState -< EvalState { 
		      current = (snd $ breakEval e) !! 0, 
		      rest = current e : (fst $ breakEval e)
		  }
		  return ()
	    where
		-- second half of pair is what we want, first is everything else
		breakEval :: EvalState -> ([Evaluator], [Evaluator])
		breakEval = break (\x -> (name x) == n) . rest

--
-- Start an evaluator
--
startEvaluator :: Data -> Name -> IO ()
startEvaluator dat@Data{txtOut=txtOut, eState=eState} name = do
	e <- getVar eState
	path <- getCompilerPath name
        case path of
            Nothing -> do
                appendText dat "Compiler not found, please install"
                return ()
            Just x -> do
                (inp,out,err,pid) <- runInteractiveCommand ("\"" ++ x ++ "\"")
                putStrLn "Starting interactive command"
                hSetBuffering out NoBuffering
                hSetBuffering err NoBuffering
                hSetBuffering inp NoBuffering
                hSetBinaryMode out True
                hSetBinaryMode err True

                appendText dat $ "\nLoading " ++ show name ++ "...\n"
                hPutStrLn inp $ (promptCmd $ current e) prompt
                hPutStrLn inp $ "putChar '\\01'"

                forkIO (readOut out)
                forkIO (readErr err)
		setHandles dat $ Just (pid,inp)
		return ()
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
-- Get the handles for the current evaluator
--
getHandles :: Data -> IO (Maybe (ProcessHandle, Handle))
getHandles dat@Data{eState=eState} = do
    e <- getVar eState
    return $ handles $ current e

--
-- Set the handles for the current evaluator
--
setHandles :: Data -> Maybe (ProcessHandle, Handle) -> IO ()
setHandles dat@Data{eState=eState} h = do
    e <- getVar eState
    eState -< e { current = (current $ e) { handles = h } }

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
