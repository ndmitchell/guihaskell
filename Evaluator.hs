
module Evaluator where


import Data
import System.IO
import System.Process
import Control.Concurrent
import PropLang.Gtk

import Text.EscapeCodes


prompt = "\x1B[0;32m%s>\x1B[0m \x1B[50m"


startEvaluator :: Data -> IO Handle
startEvaluator dat@Data{txtOut=txtOut} = do
        (inp,out,err,pid) <- runInteractiveCommand "hugs"
        putStrLn "Starting interactive command"
        hSetBuffering out NoBuffering
        hSetBuffering err NoBuffering
        hSetBuffering inp NoBuffering
        hSetBinaryMode out True
        hSetBinaryMode err True
        
        hPutStrLn inp $ ":set -p\"" ++ prompt ++ "\""
        
        forkIO (f out)
        forkIO (f err)
        return inp
    where
        g x = do threadDelay $ 10000 * 20
                 hPutStrLn x ":version"
                 g x
    
        f x = do c <- hGetContents x
                 mapM_ app $ parseEscapeCodes $ filter (/= '\r') c

        app (Left c) = appendText dat [c]
        app (Right e) = applyEscape dat e
