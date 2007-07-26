-----------------------------------------------------------------------------
-- 
-- Module      :  Prof.hs
-- Copyright   :  (c) Asumu Takikawa 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable
--
-----------------------------------------------------------------------------

module Prof where

import Control.Concurrent
import System.Directory (doesFileExist)
import System.FilePath
import System.IO
import System.Process
import Text.ParserCombinators.Parsec

import Graphics.UI.Gtk

import PropLang.Variable

import Data
import Evaluator

--
-- Run the profiler
--
runProf :: Data -> IO ()
runProf dat = do
    cF <- getVar $ profCFlags dat
    rF <- getVar $ profRFlags dat
    o  <- getVar $ executable dat
    src <- getVar $ filename dat
    case src of
      Just x -> do
	(inp, out, err, pid) <- runExternal "ghc" $ 
	    Just $ words cF ++ ["-o", o] ++ [x]
	waitForProcess pid
	let exe = if inCurrentDir o then "." </> o else o 
	(_,_,_,pid) <- runExternal exe $ Just $ words rF
	waitForProcess pid
	parseProfile dat $ o ++ ".prof"
	-- Start a new dialog here with profiling data
      Nothing -> do
	appendText dat "Profiler: No file selected\n"

    where
      inCurrentDir = null . fst . splitFileName

--
-- Parse the profiling output and open up a new dialog
-- with the parsed data
--
parseProfile dat file = do
  b <- doesFileExist file
  if b 
    then appendText dat "\n" >> readFile file >>= appendText dat
    else appendText dat "Profiler: .prof file not found\n"
  {-
  d <- dialogNew
  dialogAddButton d "gtk-close" ResponseClose
  up <- dialogGetUpper d
  l <- labelNew $ Just "Foo"
  boxPackStart up l PackNatural 0
  dialogRun d
  return ()
  -}
