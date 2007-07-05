-----------------------------------------------------------------------------
-- 
-- Module      :  Commands.hs
-- Copyright   :  (c) Asumu Takikawa 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable
--
-- Special interpreter commands
--
-----------------------------------------------------------------------------

module Commands (
	checkCommands
	) where

import Data
import Evaluator

import System.IO
-- import System.Process
import Text.ParserCombinators.Parsec

--
-- Define GuiHaskell commands
--
commands :: [String]
commands = ["hoogle", "prof", "run"]

--
-- Parses input for a GuiHaskell command
-- If parse fails, pass input to the evaluator
--
checkCommands :: Data -> String -> IO (Maybe String)
checkCommands dat inp = do 
    case parse command "" inp of
	Left err -> return $ Just inp
	Right xs -> runCommand dat xs [] >> return Nothing

--
-- Match any GuiHaskell commands?
--
command :: Parser String
command = do 
    char ':'
    foldr (<|>) pzero $ map string commands

--
-- Match command arguments
--
arguments :: Parser [String]
arguments = many anyChar `sepBy` space

--
-- Run a GuiHaskell command
--
runCommand :: Data -> String -> [String] -> IO ()
runCommand dat "hoogle" = runHoogle dat
runCommand dat "prof"   = runProf dat
runCommand dat "run"    = runRun dat
runCommand _ _		= \x -> return ()

runHoogle :: Data -> [String] -> IO ()
runHoogle dat args = do
	putStrLn "Hoogle"

runProf :: Data -> [String] -> IO ()
runProf dat args = do
	putStrLn "Prof"

runRun :: Data -> [String] -> IO ()
runRun dat args = startWithFile dat $ if null args 
					then Nothing 
					else Just $ head args
