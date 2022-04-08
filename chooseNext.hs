module Main where

{-
1 : enter a list manually
2 : get a random element from the list
3 : get and remove the element from the list, while incrementing the session count
4 : restart
4 : get a list from a file

-}

import Control.Monad (mapM)
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian, dayOfWeek, DayOfWeek(..), toGregorian)
import Data.Function ((&))
import System.IO (readFile)
import Data.List (intercalate)


data State = State
  { allDevs :: [String]
  , leftDevs :: [String]
  , sessionCount :: Int
  } 


initialize :: State
initialize =  State [] [] 0


main :: IO ()
main = do
  names <- enterDev []
  showDevs


enterDev :: [String] -> IO ([String])
enterDev names = do 
  currentDay <- currentDayOfWeek
  putStrLn "Enter next dev name or (stop)"
  nameOrSkip <- getLine
  if nameOrSkip == "stop"
    then do 
      putStrLn "Stopping choice..."
      pure names
    else enterDev $ nameOrSkip:names


showDevs :: [String] -> IO ()
showDevs devs =
  putStrLn $ intercalate ", " devs


getFromFile :: IO ()
getFromFile = do
  putStrLn "Name of the config file, leave empty for default:"
  fileName <- getLine
  fileContents <- readFile (if (fileName /= "") then fileName else "devs.txt")
  putStrLn fileContents


currentDayOfWeek :: IO DayOfWeek
currentDayOfWeek = do
  (year, month, day) <- date
  pure $ fromGregorian year month day & dayOfWeek
  where 
    date = getCurrentTime >>= return . toGregorian . utctDay