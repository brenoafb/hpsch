{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import qualified System.Console.Haskeline as Hl
import qualified Data.ByteString.Lazy as B

import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Encode.Pretty

type Repl a = Hl.InputT IO a

main :: IO ()
main = do
  tasksM <- decodeFileStrict' "tasklist" :: IO (Maybe [Task])
  case tasksM of
    Nothing -> putStrLn "Error reading input file"
    Just tasks -> do
      today <- getDay
      let (todaysTasks, remainingTasks) = L.partition (\task -> date task <= today) tasks
      putStrLn $ show (length todaysTasks) ++ " tasks allocated for today"
      putStrLn commandLine
      (done, delegated) <- Hl.runInputT Hl.defaultSettings (loop todaysTasks)
      putStrLn "Done!"
      let tasks' = remainingTasks ++ done ++ delegated
      B.writeFile "tasklist" $ encodePretty tasks'
      return ()


loop :: [Task] -> Repl ([Task], [Task])
loop = go [] []
  where go :: [Task] -> [Task] -> [Task] -> Repl ([Task], [Task])
        go done delegated [] = return (done, delegated)
        go done delegated (task:tasks) = do
          Hl.outputStr $ "\n" ++ show task ++ "\n"
          c <- Hl.getInputChar "> "
          today <- liftIO getDay
          case c of

            Just 'd' -> do
              newInfo <- (T.pack <$>) <$> Hl.getInputLine "New info: "  -- do task and schedule for later with add new description
              let task' = doTask today newInfo task
              go (task' <:> done) delegated tasks

            Just 'D' -> go (doTask today Nothing task <:> done) delegated tasks -- do task and schedule for later (maintain current description)

            Just 'x' -> go done (task:delegated) tasks                      -- delegate task for tomorrow

            Just 'l' -> go done delegated $ tasks ++ [task]                 -- push task to end of queue

            Just 'i' -> do                                                  -- adjust interval
              input <- Hl.getInputLine "New interval: "
              let nim = input >>= readMaybe
              case nim of
                Nothing -> Hl.outputStr "Invalid interval" >> go done delegated (task:tasks)
                Just ni -> let task' = task { interval = ni }
                  in go done delegated (task':tasks)

            Just '?' -> Hl.outputStr commandLine >> go done delegated (task:tasks)

            Just 'q' -> return (done, delegated ++ (task:tasks))

            Nothing -> return (done, delegated ++ (task:tasks))

            _   -> go done delegated (task:tasks)

commandLine =
  "\nd: Finish task and write new description\n\
   \D: finish task and maintain current description\n\
   \x: set task aside\n\
   \l: push task to end of queue\n\
   \i: adjust current interval\n\
   \?: show commands\n\
   \q: quit (pending tasks are delegated)"

(<:>) :: Maybe a -> [a] -> [a]
Nothing <:> xs = xs
Just x <:> xs = x:xs

printList :: Show a => [a] -> IO ()
printList xs =
  mapM_ putStr $ L.intersperse "\n\n" (map show xs) ++ ["\n"]

getDay :: IO Cal.Day
getDay = Cl.utctDay <$> Cl.getCurrentTime
