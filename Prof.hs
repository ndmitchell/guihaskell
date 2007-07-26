-- Demo code for profiler

module Prof where

import Control.Concurrent
import System.IO

import PropLang.Variable

import Data
import Evaluator
import Text.EscapeCodes

--
-- Run the profiler
--
runProf :: Data -> IO ()
runProf dat = do
    cF <- getVar $ profCFlags dat
    rF <- getVar $ profRFlags dat
    o  <- getVar $ executable dat
    (inp, out, err, pid) <- runExternal "ghc" $ Just $ words cF ++ ["-o", o]
    forkIO (readOut out)
    forkIO (readErr err)
    runExternal o $ Just $ words rF
    parseOutput $ o ++ ".prof"

    where
      parseOutput file = readFile file >>= appendText dat
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
