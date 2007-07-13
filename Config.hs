-----------------------------------------------------------------------------
-- 
-- Module      :  Config.hs
-- Copyright   :  (c) Asumu Takikawa 2007
-- License     :  
--
-- Maintainer  :  
-- Stability   :  unstable
-- Portability :  not portable, PropLang
--
-- GuiHaskell configuration API
--
-- GuiHaskell stores each configuration value in a
-- separate file inside of a configuration directory
-- (e.g. ~/.guihaskell/config)
--
-----------------------------------------------------------------------------

module Config (
	confInit, newConfValue, newConfValueWithDefault
	) where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

import PropLang.Event
import PropLang.Value

--
-- Make sure we can do config stuff
--
confInit :: IO ()
confInit = do
    d <- getConfigDir
    createDirectoryIfMissing True d

--
-- A value constructor that reads and writes using a
-- set of config files
--
newConfValue :: (Eq a, Show a, Read a) => String -> Event -> IO (Value a)
newConfValue = newConfValueWithDefault ""

--
-- Config constructor that takes a default value
--
newConfValueWithDefault 
    :: (Eq a, Show a, Read a) => String -> String-> Event -> IO (Value a)
newConfValueWithDefault def name ev = do
    cf <- confFile
    exists <- doesFileExist cf
    if exists then do return ()
	      else do openFile cf WriteMode >>= hClose -- touch
    return $ Value (setter) (getter)
    where
      setter x = do
	  cf <- confFile
	  old <- getter
	  h <- openFile cf WriteMode
	  hPutStr h $ show x
	  hClose h
	  if old /= x then do raise ev
		      else do return ()

      getter = do
	  cf <- confFile
	  h <- openFile cf ReadMode
	  val <- read `liftM` catch (hGetLine h)
		  (\e -> if isEOFError e then return $ show def else ioError e)
	  hClose h
	  return val

      confFile = do d <- getConfigDir; return $ d </> name

-- 
-- The directory where config files reside
-- Might need special case for Windows?
--
getConfigDir :: IO FilePath
getConfigDir = do
    d <- getHomeDirectory
    return $ d </> ".guihaskell" </> "config"
