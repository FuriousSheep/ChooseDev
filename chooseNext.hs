module Main where

{-
1 : enter a list manually - DONE
2 : get a random element from the list - DONE
3 : get and remove the element from the list, while incrementing the session count - DONE
4 : restart - DONE
5 : get and display file argument - DONE
6 : get a list of devs from a file
7 : fix negative index bug when getting too few devs
-}

import Control.Monad (mapM)
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian, dayOfWeek, DayOfWeek(..), toGregorian)
import Data.Function ((&))
import System.IO (readFile)
import System.Environment (getArgs)
import System.Random (StdGen(..), initStdGen, randomR)
import Data.List (intercalate)
import Data.List.Index (deleteAt)


data State = State [String] [String] Int

main :: IO ()
main = do
  args <- getArgs
  mapM putStrLn args
  names <- enterDev []
  (State _ _ sessions) <- chooseDev (State names names 1)
  putStrLn $ "Good job on your " ++ show sessions ++ " sessions!" 


enterDev :: [String] -> IO ([String])
enterDev names = do 
  currentDay <- currentDayOfWeek
  putStrLn "Enter next dev name or (stop)"
  nameOrSkip <- getLine
  if nameOrSkip == "stop"
    then do 
      putStrLn "Stopping choice..."
      pure names
    else enterDev (nameOrSkip:names)


chooseDev :: State -> IO (State)
chooseDev (State allDevs leftDevs sessionCount) = do
  gen <- (initStdGen :: IO StdGen)
  index <- pure $ fst $ randomDev gen leftDevs
  updatedLeftDevs <- pure $ deleteAt index leftDevs
  putStrLn $ "Session " ++ show sessionCount ++ "."
  putStrLn $ "Scribe: " ++ show (leftDevs !! index)
  showDevs updatedLeftDevs
  putStrLn "Continue? (y/n)"
  continue <- getLine
  handleContinue continue updatedLeftDevs 
    where
      handleContinue "y" withLeftDevs = 
        if withLeftDevs == [] 
          then chooseDev (State allDevs allDevs (sessionCount + 1))
          else chooseDev (State allDevs withLeftDevs (sessionCount + 1))
      handleContinue "n" withLeftDevs = pure $ State allDevs withLeftDevs sessionCount
      handleContinue _ withLeftDevs = do
        putStrLn "Command not recognized. Continue? (y/n)"
        continueEntry <- getLine
        handleContinue continueEntry withLeftDevs
      showDevs [] = do
        putStrLn $ "No devs left, all previous devs added to new turn."
        putStrLn $ "Devs left this turn: " ++ intercalate ", " allDevs ++ "."
      showDevs devs =
        putStrLn $ "Devs left this turn: " ++ intercalate ", " devs ++ "."


randomDev :: StdGen -> [String] -> (Int, StdGen)
randomDev gen devs =
  randomR (0, length devs - 1) gen

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