module Main where

import MyLib

main :: IO ()
main = do
  print $ run startDb user1 variable1 action1
