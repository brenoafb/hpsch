module Main where

import Lib

import qualified Data.List as L

printList :: Show a => [a] -> IO ()
printList xs =
  mapM_ putStr $ L.intersperse "\n\n" (map show xs) ++ ["\n"]

main :: IO ()
main = printList exampleTasks
