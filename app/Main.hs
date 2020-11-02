{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import qualified System.Console.Haskeline as Hl

import Control.Monad.Trans

type Repl a = Hl.InputT IO a

main :: IO ()
main = do
  putStrLn "All tasks:\n"
  printList exampleTasks
  putStrLn "\nTasks allocated for today:\n"
  d <- today
  let todaysTasks = overdueTasks d exampleTasks ++ daysTasks d exampleTasks
  printList todaysTasks
  putStrLn ""
  (done, delegated) <- Hl.runInputT Hl.defaultSettings (loop todaysTasks)
  putStrLn "Congratulations, you're done!"

loop :: [Task] -> Repl ([Task], [Task])
loop = go [] []
  where go :: [Task] -> [Task] -> [Task] -> Repl ([Task], [Task])
        go done delegated [] = return (done, delegated)
        go done delegated (task:tasks) = do
          Hl.outputStr $ show task ++ "\n"
          c <- Hl.getInputChar "> "
          day <- liftIO today
          case c of
            Just 'd' -> do
              newInfo <- (T.pack <$>) <$> Hl.getInputLine "New info: "              -- do task and schedule for later with add new description
              let task' = doTask day newInfo task
              go (task' <:> done) delegated tasks
            Just 'D' -> go (doTask day Nothing task <:> done) delegated tasks -- do task and schedule for later (maintain current description)
            Just 'x' -> go done (task:delegated) tasks                      -- delegate task for tomorrow
            Just 'l' -> go done delegated $ tasks ++ [task]                 -- push task to end of queue
            _   -> go done delegated (task:tasks)

(<:>) :: Maybe a -> [a] -> [a]
Nothing <:> xs = xs
Just x <:> xs = x:xs

exampleTasks :: [Task]
exampleTasks =
  [ Task "HPSched" Medium (Cal.fromGregorian 2020 11 2) Nothing "Tinker with HPSched prototype"
  , Task "Electronics Project" High (Cal.fromGregorian 2020 11 2) (Just $ Cal.fromGregorian 2020 11 3) "Complete simulations of 2nd supply project"
  , Task "OS Project" Low (Cal.fromGregorian 2020 11 1) (Just $ Cal.fromGregorian 2020 11 25) "Check out the description for OS class project #2"
  , Task "Nielsen's DL Book" Low (Cal.fromGregorian 2020 11 4) Nothing "Continue reading second half of chapter 2"
  ]

printList :: Show a => [a] -> IO ()
printList xs =
  mapM_ putStr $ L.intersperse "\n\n" (map show xs) ++ ["\n"]

today :: IO Cal.Day
today = Cl.utctDay <$> Cl.getCurrentTime
