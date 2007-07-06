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

type Command = Data -> [String] -> IO ()

--
-- Define GuiHaskell commands
--
commands :: [(String, Command)]
commands = 
    [ ("hello", runHello)
    , ("hoogle", runHoogle)
    , ("prof", runProf)
    , ("run", runRun)
    ]

--
-- Parses input for a GuiHaskell command
-- If parse fails, pass input to the evaluator
--
checkCommands :: Data -> String -> IO (Maybe String)
checkCommands dat inp = do 
    case parse command "" inp of
	Left err -> return $ Just inp
	Right (c, as) -> runCommand dat c as >> return Nothing

--
-- Match any GuiHaskell commands?
--
command :: Parser (String, [String])
command = do 
    char ':'
    c <- foldr (<|>) pzero $ map (try . string . fst) commands
    as <- arguments
    return (c, as)

--
-- Match command arguments
--
arguments :: Parser [String]
arguments = many anyChar `sepBy` space

--
-- Run a GuiHaskell command
--
runCommand :: Data -> String -> [String] -> IO ()
runCommand dat str args =
    let command = lookup str commands in
    case command of
	Nothing -> return ()
	Just x  -> x dat args

-- 
-- Proof of concept function
--
runHello :: Data -> [String] -> IO ()
runHello dat arg = runExternal "echo" (Just ["Hello World!"]) (\x -> hGetContents x >>= putStrLn) (\x -> return ())

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
