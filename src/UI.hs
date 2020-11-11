{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lib

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Console.Haskeline as Hl
import qualified Data.ByteString.Lazy as B

import Data.Time
import Text.Read (readMaybe)

import Control.Monad.State

data TaskState = TaskState
  { done :: [Task]
  , delegated :: [Task]
  , current :: [Task]
  } deriving (Eq, Show)

data Action = Action
  { key :: Char
  , description :: T.Text
  , handler :: Repl () -> Repl ()
  }

instance Show Action where
  show action = key action : "\t" ++ d
    where d = T.unpack $ description action

type Repl a = StateT TaskState (Hl.InputT IO) a

loop :: Repl ()
loop = do
  tasks <- gets current
  if null tasks
     then return ()
     else do
       let task = head tasks
       lift (Hl.outputStr $ "\n" ++ show task ++ "\n")
       cm <- lift $ Hl.getInputChar "> "
       case cm of
         Nothing -> loop
         Just c -> case M.lookup c actions of
           Nothing -> loop
           Just action -> handler action loop

commandLine :: T.Text
commandLine = T.unlines $ map (\(k, action) -> T.pack $ show action) $ M.toList actions


buildActionMap :: [Action] -> M.Map Char Action
buildActionMap = M.fromList . map (\action -> (key action, action))

(<:>) :: Maybe a -> [a] -> [a]
Nothing <:> xs = xs
Just x <:> xs = x:xs

printList :: Show a => [a] -> IO ()
printList xs =
  mapM_ putStr $ L.intersperse "\n\n" (map show xs) ++ ["\n"]

getDay :: IO Day
getDay = localDay . zonedTimeToLocalTime <$> getZonedTime

actions :: M.Map Char Action
actions = buildActionMap
  [ Action
      { key = 'd'
      , description = "Complete task and edit task description"
      , handler = \cont -> do
          tasks' <- gets current
          if null tasks' then cont
            else do
              let (task:tasks) = tasks'
              newInfo <- lift $ (T.pack <$>) <$> Hl.getInputLine "New info: "
              today <- liftIO getDay
              let task' = doTask today newInfo task
              modify $ \taskState -> taskState { done = task' <:> done taskState, current = tasks }
              cont
      }
  , Action
     { key = 'D'
      , description = "Complete current task"
      , handler = \cont -> do
          tasks' <- gets current
          if null tasks' then cont
          else do
            let (task:tasks) = tasks'
            today <- liftIO getDay
            let task' = doTask today Nothing task
            modify $ \taskState -> taskState { done = task' <:> done taskState, current = tasks }
            cont
     }
  , Action
      { key = 'x'
      , description = "Delegate task for tomorrow"
      , handler = \cont -> do
          tasks' <- gets current
          if null tasks' then cont
            else do
              let (task:tasks) = tasks'
              taskState <- get
              put $ taskState { delegated = task : delegated taskState , current = tasks}
              cont

      }
  , Action
      { key = 'l'
      , description = "Push current task to the end of the queue"
      , handler = \cont -> do
          tasks' <- gets current
          if null tasks' then cont
            else do
              let (task:tasks) = tasks'
              taskState <- get
              put $ taskState { current = tasks ++ [task] }
              cont

      }
  , Action
      { key = 'i'
      , description = "Adjust current task's interval"
      , handler = \cont -> do
          tasks' <- gets current
          if null tasks' then cont
            else do
              let (task:tasks) = tasks'
              input <- lift $ Hl.getInputLine "New interval: "
              let nim = input >>= readMaybe
              case nim of
                Nothing -> lift (Hl.outputStr "Invalid interval") >> cont
                Just ni -> do
                  let task' = task { interval = ni }
                  modify $ \taskState -> taskState { current = task':tasks }
                  cont

      }
  , Action
      { key = '?'
      , description = "Show commands"
      , handler = \cont -> do
          lift $ Hl.outputStr $ T.unpack commandLine
          cont
      }
  , Action
      { key = 'q'
      , description = "Quit (current and queued tasks are delegated)"
      , handler = \cont ->
          modify $ \taskState -> taskState { delegated = current taskState, current = [] }
      }
  ]
