{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import UI

import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Console.Haskeline as Hl
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Time
import System.Directory


main :: IO ()
main = do
  home <- getHomeDirectory
  let tasklistFile = home ++ "/.local/hpsch/tasklist"
      backupsDir   = home ++ "/.local/hpsch/backups/"
  tasksM <- decodeFileStrict' tasklistFile :: IO (Maybe [Task])
  case tasksM of
    Nothing -> putStrLn "Error reading input file"
    Just tasks -> do
      today <- getDay
      let (todaysTasks, remainingTasks) = L.partition (\task -> date task <= today) tasks
      putStrLn $ show (length todaysTasks) ++ " tasks allocated for today"
      let taskState = TaskState [] [] todaysTasks
      taskState' <- Hl.runInputT Hl.defaultSettings $ execStateT loop taskState
      putStrLn "Done!"
      let tasks' = remainingTasks ++ done taskState' ++ delegated taskState'
      timestamp <- getTimestamp
      B.writeFile (backupsDir ++ "tasklist-backup-" ++ timestamp) $ encodePretty tasks
      B.writeFile tasklistFile $ encodePretty tasks'
      return ()

getTimestamp :: IO String
getTimestamp =
  formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getCurrentTime
