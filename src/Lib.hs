{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Data.Maybe (fromMaybe)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty

data Task = Task
  { name :: Name
  , interval :: Integer
  , date :: Date
  , deadline :: Maybe Date
  , info :: Info
  } deriving (Generic, Eq)

type Name = T.Text

type Date = Cal.Day

type Info = T.Text

instance Show Task where
  show t = T.unpack text
    where
      deadlineStr = maybe "" (\x -> "\nDeadline: " <> T.pack (show x)) (deadline t)
      text = name t <> " - " <> info t
        <> "\nInterval: " <> T.pack (show $ interval t)
        <> "\nNext date: " <> T.pack (show $ date t)
        <> deadlineStr

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

overdueTasks :: Date -> [Task] -> [Task]
overdueTasks date' =
  filter (\task -> date task < date')

daysTasks :: Date -> [Task] -> [Task]
daysTasks date' =
  filter (\task -> date task < date')

-- Execute a given task on a given day
-- If there is a deadline and the new date for
-- the task surpasses the deadline, then Nothing is returned
-- If the new date does not surpass the deadline or
-- there is no deadline, the task with the date and information
-- adjusted is returned
doTask :: Cal.Day -> Maybe Info -> Task -> Maybe Task
doTask d mi task =
  let nd' = Cal.addDays (interval task) d
      -- i' = maybe (info task) id mi
      i' = fromMaybe (info task) mi
   in case (>= nd') <$> deadline task of
        Just False -> Nothing
        _ -> Just $ task { date = nd', info = i' }
