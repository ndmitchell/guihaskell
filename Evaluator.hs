
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


setCompiler :: Maybe String -> Var String -> IO ()
setCompiler str compiler =
    case str of
	Nothing -> do
	    compiler -< defaultCompiler
	Just x -> do
	    compiler -< x
    where defaultCompiler = "Hugs"

startEvaluator :: Data -> IO (Maybe (ProcessHandle, Handle))
startEvaluator dat@Data{txtOut=txtOut,compiler=compiler} = do
	useHugs <- isHugs
        hugs <- if useHugs then getHugsPath else getGHCiPath
        case hugs of
            Nothing -> do
                appendText dat "Hugs not found, please install"
                return Nothing
            Just x -> do
                (inp,out,err,pid) <- runInteractiveCommand ("\"" ++ x ++ "\"")
                putStrLn "Starting interactive command"
                hSetBuffering out NoBuffering
                hSetBuffering err NoBuffering
                hSetBuffering inp NoBuffering
                hSetBinaryMode out True
                hSetBinaryMode err True

                appendText dat $ "Loading " ++ if useHugs then "Hugs..." else "GHCi...\n"
                hPutStrLn inp $ if useHugs
                                then ":set -p\"" ++ prompt ++ "\""
                                else ":set prompt " ++ prompt
                hPutStrLn inp $ "putChar '\\01'"

                forkIO (readOut out)
                forkIO (readErr err)
                return $ Just (pid,inp)
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

	isHugs = do
	    compStr <- getVar compiler
	    case compStr of
		"Hugs" -> return True
		_      -> return False
	    



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

