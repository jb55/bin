{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Control.Monad.IO.Class
import Data.List
import Safe
import System.Cmd
import System.Console.CmdArgs
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.IO
import Text.Printf

data Brent = Install
             { pkg        :: String
             , instArgs   :: [String]
             }
           | Switch
             { prog       :: String
             , versionNum :: Maybe String
             , stow       :: Maybe FilePath
             }
  deriving (Data, Typeable, Show)

installMode :: Brent
installMode = Install { pkg = def &= argPos 0 &= typ "PACKAGE"

                      , instArgs = def &= args &= typ "FLAGS"
                      }

switchMode :: Brent
switchMode = Switch { prog       = "ghc" &= typ "PROGRAM"
                    , versionNum = def &= typ "VERSION"
                    , stow       = def
                                   &= typ "DIR"
                                   &= help "Location of stowed packages"
                    }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  a <- cmdArgs (modes [installMode, switchMode])
  case a of
    Install p args    -> tryInstall p args
    Switch p ver sDir -> do
      env <- getEnvironment
      let stowEnv = lookup "STOW" env
          sDir'   = case (stowEnv, sDir) of
                    (_, Just d) -> d
                    (Just d, _) -> d
                    _           -> "/scratch/local/stow"
      switch p ver sDir'

------------------------------------------------------------
--  Utilities
------------------------------------------------------------

(==>) :: a -> b -> (a,b)
(==>) = (,)

choices :: MonadIO m => [(String, m ())] -> m ()
choices cs = do
  inp <- liftIO $ do
    printChoices (map fst cs)
    putStr "> "
    getLine
  case readMay inp of
    Just n  -> snd (cs !! n)
    Nothing -> choices cs

printChoices :: [String] -> IO ()
printChoices = mapM_ printChoice . zip [0::Int ..]
  where printChoice (i, s) = putStrLn (printf "%2d" i ++ ") " ++ s)

------------------------------------------------------------
--  Install
------------------------------------------------------------

tryInstall :: String -> [String] -> IO ()
tryInstall p args = do
  let args' = unwords args
  exit <- system $ "cabal install --dry-run " ++ p ++ " " ++ args' ++ " | highlight-versions"
  case exit of
    ExitSuccess -> do
      choices [ "Proceed with installation" ==> doInstall p args
              , "Another dry run"           ==> tryInstall p args
              , "Quit"                      ==> return ()
              ]
    _           -> do
      choices [ "Try dry run again" ==> tryInstall p args
              , "Quit"              ==> return ()
              ]

doInstall :: String -> [String] -> IO ()
doInstall p args = do
  let args' = unwords args
  exit <- system $ "cabal install " ++ p ++ " " ++ args'
  case exit of
    ExitSuccess -> return ()
    _           -> do
      choices [ "Another dry run" ==> tryInstall p args
              , "Try install again" ==> doInstall p args
              , "Quit" ==> return ()
              ]

------------------------------------------------------------
--  Stow switch
------------------------------------------------------------

switch :: String -> Maybe String -> FilePath -> IO ()
switch p (Just ver) sDir = doSwitch p ver sDir
switch p Nothing    sDir = do
  fs <- namesMatching $ sDir </> (p ++ "-*")
  let vers = sort . map (drop (length p + 1) . takeFileName) $ fs
  choices $ ("Quit", return ()) : map (\v -> (v, doSwitch p v sDir)) vers

doSwitch :: String -> String -> FilePath -> IO ()
doSwitch p ver sDir = do
  _ <- system $ "cd " ++ sDir ++ " && stow -D \"" ++ p ++ "-`" ++ p ++ " --numeric-version`\" && stow \"" ++ p ++ "-" ++ ver ++ "\""
  return ()