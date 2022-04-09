module State where

import System.IO as IO
import System.IO.Error as IO.Error
import Data.List (intercalate)

data State = State [String] [String] Int

fromArgs :: [String] -> IO State
fromArgs [] = do
  pure $ State [] [] (-1)
fromArgs [fileName] = do  
  state@(State devNames _ _) <- fromFile  fileName
  showDevs devNames
  pure state
fromArgs devNames = do
  mapM putStrLn devNames
  showDevs devNames
  pure $ State devNames devNames 0

fromFile :: String -> IO State
fromFile fileName = do
  contents <- IO.readFile fileName
  devNames <- pure $ lines contents
  pure $ State devNames devNames 0

showDevs :: [String] -> IO ()
showDevs devs = 
  putStrLn $ "Devs for this mob session: " ++ (intercalate ", " devs) ++ "."