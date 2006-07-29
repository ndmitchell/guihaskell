
module Evaluator where


import Data
import Data.Char
import System.IO
import System.Process
import System.Directory
import Control.Concurrent
import PropLang.Gtk

import Text.EscapeCodes


prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"


startEvaluator :: Data -> IO (Maybe Handle)
startEvaluator dat@Data{txtOut=txtOut} = do
        hugs <- getHugsPath
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

                appendText dat "Loading Hugs..."
                hPutStrLn inp $ ":set -p\"" ++ prompt ++ "\""
                hPutStrLn inp $ "putChar '\\01'"

                forkIO (f out)
                forkIO (f err)
                return $ Just inp
    where
        g x = do threadDelay $ 10000 * 20
                 hPutStrLn x ":version"
                 g x
    
        f x = do c <- hGetContents x
                 mapM_ app $ parseEscapeCodes $ filter (/= '\r') $ tail $ dropWhile (/= '\01') c

        app (Left c) = appendText dat [c]
        app (Right e) = applyEscape dat e



getHugsPath :: IO (Maybe FilePath)
getHugsPath = do
    path <- findExecutable "hugs.exe"
    case path of
        Just x -> return $ Just x
        Nothing -> do
            let guesses = ["c:\\Program Files\\WinHugs\\hugs.exe"]
            res <- mapM doesFileExist guesses
            let ans = map snd $ filter fst $ zip res guesses
            return $ if null ans then Nothing else Just (head ans)

