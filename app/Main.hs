{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import UI

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
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

main :: IO ()
main = do
  tasksM <- decodeFileStrict' "tasklist" :: IO (Maybe [Task])
  case tasksM of
    Nothing -> putStrLn "Error reading input file"
    Just tasks -> do
      today <- getDay
      let (todaysTasks, remainingTasks) = L.partition (\task -> date task <= today) tasks
      putStrLn $ show (length todaysTasks) ++ " tasks allocated for today"
      -- putStrLn commandLine
      -- (done, delegated, _) <- Hl.runInputT Hl.defaultSettings $ execStateT (runRepl loop) ([], [], todaysTasks)
      let taskState = TaskState [] [] todaysTasks
      taskState' <- Hl.runInputT Hl.defaultSettings $ execStateT loop taskState
      putStrLn "Done!"
      let tasks' = remainingTasks ++ done taskState' ++ delegated taskState'
      B.writeFile "tasklist" $ encodePretty tasks'
      return ()
