{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal

data Task = Task Name Priority Date (Maybe Date) Info
  deriving Eq

type Name = T.Text

type Date = Cal.Day

data Priority = Low | Medium | High
  deriving (Ord, Eq, Show)

type Info = T.Text

instance Show Task where
  show (Task n p nd dl i) = let
    y = n <> " - " <> i
          <> "\nPriority: " <> T.pack (show p)
          <> "\nNext date: " <> T.pack (show nd)
          <> "\nDeadline: " <> T.pack (show dl)
    in T.unpack y

overdueTasks :: Date -> [Task] -> [Task]
overdueTasks date =
  filter (\(Task _ _ nd _ _) -> nd < date)

daysTasks :: Date -> [Task] -> [Task]
daysTasks date =
  filter (\(Task _ _ nd _ _) -> nd == date)

priority2days :: Priority -> Integer
priority2days Low = 7
priority2days Medium = 3
priority2days High = 1

-- Execute a given task on a given day
-- If there is a deadline and the new date for
-- the task surpasses the deadline, then Nothing is returned
-- If the new date does not surpass the deadline or
-- there is no deadline, the task with the date and information
-- adjusted is returned
doTask :: Cal.Day -> Maybe Info -> Task -> Maybe Task
doTask d mi (Task n p nd dl i) =
  let nd' = Cal.addDays (priority2days p) d
      i' = maybe i id mi
   in case (> nd') <$> dl of
        Nothing -> Just $ Task n p nd' dl i'
        Just True -> Just $ Task n p nd' dl i'
        Just False -> Nothing
