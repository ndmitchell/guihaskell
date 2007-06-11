
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


setCompiler :: Maybe String -> Var Compiler -> IO ()
setCompiler m var =
    case m of
	Nothing -> do
	    var -< defaultCompiler
	Just x -> do
	    var -< read x
    where defaultCompiler = Hugs

startEvaluator :: Data -> IO ()
startEvaluator dat@Data{txtOut=txtOut,compiler=compiler,cHandles=cHandles} = do
	c <- getVar compiler
	path <- getCompilerPath c
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

                appendText dat $ "Loading " ++ show c ++ "...\n"
                hPutStrLn inp $ case c of
				  Hugs -> ":set -p\"" ++ prompt ++ "\""
				  GHCi -> ":set prompt " ++ prompt
                hPutStrLn inp $ "putChar '\\01'"

                forkIO (readOut out)
                forkIO (readErr err)
		cHandles -< Just (pid,inp)
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
stopEvaluator dat@Data{cHandles=cHandles} = do
    handles <- getVar cHandles
    case handles of
	Nothing -> return ()
	Just (pid,inp) -> do
	    hPutStrLn inp "\n:quit\n"
	    waitForProcess pid
	    return ()	

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
