{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text as T
import Data.Time.Calendar as Cal

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

exampleTasks :: [Task]
exampleTasks =
  [ Task "HPSched" Medium (Cal.fromGregorian 2020 11 1) Nothing "Tinker with HPSched prototype"
  , Task "Electronics Project" High (Cal.fromGregorian 2020 11 1) (Just $ Cal.fromGregorian 2020 11 3) "Complete simulations of the rectifier module"
  , Task "OS Project" Low (Cal.fromGregorian 2020 11 1) (Just $ Cal.fromGregorian 2020 11 25) "Check out the description for OS class project #2"
  ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
