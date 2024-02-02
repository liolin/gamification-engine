module Main where

import MyLib

main :: IO ()
main = do
  print $ dbStates startDb
  let db = run startDb user1 variable1 action1
  print $ dbStates db
  print $ dbStates $ run db user1 variable1 action1

