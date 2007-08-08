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
import Data.Char
import Data.Tree
import System.Directory (doesFileExist)
import System.FilePath
import System.IO
import System.Process
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView.CellLayout
import qualified Graphics.UI.Gtk.ModelView as MView

import PropLang.Variable

import Data
import Evaluator
import Util

data Profile = Profile
               { title :: String
	       , flags :: String
	       , time :: String
	       , alloc :: String
	       }

data ProfileLine = ProfileLine 
		   { costCentre :: String
		   , moduleName :: String
		   , entries :: Integer
		   , indvTime :: Double
		   , indvAlloc :: Double
		   , inhTime :: Double
		   , inhAlloc :: Double 
		   }

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
	exist <- doesFileExist x
	if not exist
	  then errorMessage dat "Selected file does not exist."
	  else do
	    (inp, out, err, pid) <- runExternal "ghc" $ 
	        Just $ words cF ++ ["-o", o] ++ [x]
	    waitForProcess pid
	    let exe = if inCurrentDir o then "." </> o else o 
	    (_,_,_,pid) <- runExternal exe $ Just $ words rF
	    waitForProcess pid
	    res <- runProfileParser $ o ++ ".prof"
	    case res of
	      Left s  -> errorMessage dat s 
	      Right x -> runParseDialog x
      Nothing -> do
	errorMessage dat "No file selected for profiling."

    where
      inCurrentDir = null . fst . splitFileName

-- Parse a line
parseProfileLine :: Parser ProfileLine
parseProfileLine = do 
    let lexer = makeTokenParser haskellDef
    spaces
    cc <- notSpaces     ; spaces
    mn <- notSpaces     ; spaces
    no <- natural lexer ; spaces
    en <- natural lexer ; spaces
    it <- float lexer   ; spaces
    ia <- float lexer   ; spaces
    iht <- float lexer  ; spaces
    iha <- float lexer
    return $ ProfileLine cc mn en it ia iht iha

    where
	notSpaces = many $ satisfy $ not . isSpace

-- Parse the profiling output
parseProfile :: Parser [ProfileLine]
parseProfile = do
    ls <- many1 parseProfileLine
    return ls

-- Run the profile parsers
runProfileParser :: FilePath -> IO (Either String (Profile, [ProfileLine]))
runProfileParser file = do
  b <- doesFileExist file
  if not b 
    then return $ Left "No .prof file was found. Check your profile flags."
    else do
      contents <- readFile file 
      let (title:_:flags:_:time:alloc:rest) = lines contents
	  prof = Profile (dropWhile isSpace title) (dropWhile isSpace flags)
	                 (dropWhile isSpace time) (dropWhile isSpace alloc)
      case parse parseProfile "" (unlines $ drop 11 $ rest) of
	Left x  -> return $ Left $ "Parse error: " ++ show x ++ "\n\n" ++
				    "This is likely a bug. Please file a report."
	Right x -> return $ Right (prof, x)

-- Set up and display the profiling dialog
runParseDialog :: (Profile, [ProfileLine]) -> IO ()
runParseDialog (p, ls) = do
    d <- dialogNew
    dialogAddButton d "gtk-close" ResponseClose
    up <- dialogGetUpper d
    titleLabel <- labelNew $ Just $ title p
    flagLabel  <- labelNew $ Just $ flags p
    timeLabel  <- labelNew $ Just $ time p
    allocLabel <- labelNew $ Just $ alloc p
    
    view <- MView.treeViewNew
    store <- MView.treeStoreNew []
    MView.treeViewSetModel view store

    -- Thanks to the Gtk2hs folks for this
    let createTextColumn name field = do
	  column <- MView.treeViewColumnNew
	  MView.treeViewAppendColumn view column
	  MView.treeViewColumnSetTitle column name
	  cell <- cellRendererTextNew
	  MView.treeViewColumnPackStart column cell True
	  cellLayoutSetAttributes column cell store
	    (\record -> [MView.cellText := field record])
	  
    createTextColumn "Cost Centre"       costCentre
    createTextColumn "Module"            moduleName
    createTextColumn "Entries"           (show . entries)
    createTextColumn "Individual %time"  (show . indvTime)
    createTextColumn "Individual %alloc" (show . indvAlloc)
    createTextColumn "Inherited %time"   (show . inhTime)
    createTextColumn "Inherited %alloc"  (show . inhAlloc)

    mapM_ (MView.treeStoreInsert store [] 0) ls

    boxPackStart up titleLabel PackNatural 0
    boxPackStart up flagLabel PackNatural 0
    boxPackStart up timeLabel PackNatural 0
    boxPackStart up allocLabel PackNatural 0
    boxPackStart up view PackRepel 0
    
    widgetShowAll d
    dialogRun d
    widgetHide d
    return ()
