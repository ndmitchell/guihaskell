
module Evaluator where


import Data
import Data.Char
import qualified Data.Map as M
import System.IO
import System.Process
import System.Directory
import Control.Concurrent
import PropLang.Gtk
import PropLang.Variable

import Text.EscapeCodes


prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"


setCompiler :: Maybe String -> Var Compiler -> IO ()
setCompiler m var =
    case m of
	Nothing -> do
	    var -< defaultCompiler
	Just x -> do
	    var -< read x
    where defaultCompiler = Hugs

switchEvaluator :: Data -> IO ()
switchEvaluator dat = do
    pair <- getHandles dat
    case pair of
	Nothing -> do
	    startEvaluator dat
	Just x -> do
	    return ()

startEvaluator :: Data -> IO ()
startEvaluator dat@Data{txtOut=txtOut,selection=selection,compilers=compilers} = do
	s <- getVar selection
	cMap <- getVar compilers
	path <- getCompilerPath s
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

                appendText dat $ "Loading " ++ show s ++ "...\n"
                hPutStrLn inp $ case s of
				  Hugs -> ":set -p\"" ++ prompt ++ "\""
				  GHCi -> ":set prompt " ++ prompt
                hPutStrLn inp $ "putChar '\\01'"

                forkIO (readOut out)
                forkIO (readErr err)
		compilers -< M.update (\x -> Just (pid,inp)) s cMap
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

stopEvaluator :: Data -> IO ()
stopEvaluator dat = do
    handles <- getHandles dat
    case handles of
	Nothing -> return ()
	Just (pid,inp) -> do
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()

getHandles :: Data -> IO (Maybe (ProcessHandle, Handle))
getHandles dat@Data {selection=selection, compilers=compilers} = do
    s <- getVar selection
    cMap <- getVar compilers
    return $ M.lookup s cMap

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

getOtherPath :: Compiler -> IO (Maybe FilePath)
getOtherPath = findExecutable . map toLower . show
