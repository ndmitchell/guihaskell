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

import System.IO
-- import System.Process
import Text.ParserCombinators.Parsec

--
-- Define GuiHaskell commands
--
commands :: [String]
commands = ["hoogle", "prof"]

--
-- Parses input for a GuiHaskell command
-- If parse fails, pass input to the evaluator
--
checkCommands :: Data -> String -> IO (Maybe String)
checkCommands dat inp = do 
    case parse command "" inp of
	Left err -> return $ Just inp
	Right xs -> runCommand dat xs >> return Nothing

--
-- Match any GuiHaskell commands?
--
command :: Parser String
command = do 
    char ':'
    foldr (<|>) pzero $ map string commands

--
-- Run a GuiHaskell command
--
runCommand :: Data -> String -> IO ()
runCommand dat "hoogle" = runHoogle dat
runCommand dat "prof"   = runProf dat
runCommand _ _		= return ()

runHoogle :: Data -> IO ()
runHoogle dat = do
	putStrLn "Hoogle"

runProf :: Data -> IO ()
runProf dat = do
	putStrLn "Prof"
