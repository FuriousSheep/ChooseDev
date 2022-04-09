module Main where

{-
1 : enter a list manually - DONE
2 : get a random element from the list - DONE
3 : get and remove the element from the list, while incrementing the session count - DONE
4 : restart - DONE
5 : get and display file argument - DONE
6 : get a list of devs from a file - DONE
7 : fix negative index bug when getting too few devs -- NO LONGER APPLICABLE
8 : ???
9 : PROFIT
10: Add dayOfWeek feature
-}

import Data.Time.Clock as Clock
import Data.Time.Calendar as Calendar
import Data.Function ((&))
import System.IO as IO
import System.Environment as Environment 
import Data.List (intercalate)
import Data.List.Index (deleteAt)
import State
import Random
import System.Random (StdGen(..), initStdGen)

main :: IO ()
main = do
  state <- State.fromArgs =<< Environment.getArgs
  runDevChoice state

runDevChoice :: State -> IO ()
runDevChoice state = do
  (State allDevs leftDevs rotations) <- myUntil sessionEnds state incrementSession
  case (State allDevs leftDevs rotations) of
    (State _ _ (-1)) -> do
      printHowToUse
    (State _ _ 0) -> do putStrLn $ "Thanks for booting this program before being ready è_é" 
    (State _ _ n) -> do putStrLn $ "Congratulations on your " ++ (show n) ++ " rotations!"
  where
    sessionEnds (State allDevs _ _) = 
      allDevs == []
    incrementSession (State allDevs leftDevs rotations)
      | leftDevs == [] = do
        chooseDev $ State allDevs allDevs rotations
      | otherwise = do
        chooseDev $ State allDevs leftDevs rotations
    printHowToUse = do
      putStrLn "How to use:"
      putStrLn " $ ./chooseNext"
      putStrLn " -- shows this information"
      putStrLn ""
      putStrLn " $ ./chooseNext dev1 dev2 ... nthdev"
      putStrLn " -- for manual entry of the devs"
      putStrLn ""
      putStrLn " $ ./chooseNext fileName"
      putStrLn " -- for using devs stored in a file"
      putStrLn " -- the file should contain only one dev per line"
      putStrLn ""


myUntil :: (State -> Bool) -> State -> (State -> IO State) -> IO State
myUntil _ state@(State allDevs leftDevs (-1)) _ = do
  pure state
myUntil stop state@(State allDevs leftDevs rotations) updateState = do
  if stop state 
    then do
      putStrLn "Stopping session"
      pure state
    else do
      putStrLn "Next? (y/n)"
      continue <- IO.getLine
      case continue of
        "y" -> do 
          newState <- (updateState state) 
          myUntil stop newState updateState
        "n" -> do 
          pure state
        _ -> do
          putStrLn "Invalid input"
          myUntil stop state updateState

chooseDev :: State.State -> IO (State.State)
chooseDev (State allDevs leftDevs rotations) = do
  gen <- (initStdGen :: IO StdGen)
  index <- pure $ fst $ Random.randomDev gen leftDevs
  putStrLn $ "Scribe: " ++ (leftDevs !! index)
  updatedLeftDevs <- pure $ deleteAt index leftDevs
  putStrLn $ case updatedLeftDevs of
    [] ->
      "No devs left for this session, bringing everyone back: " ++ intercalate ", " allDevs
    someDevs -> 
      "Remaining devs for this session: " ++ intercalate ", " someDevs
  pure $ State allDevs updatedLeftDevs (rotations + 1)

currentDayOfWeek :: IO Calendar.DayOfWeek
currentDayOfWeek = do
  (year, month, day) <- date
  pure $ Calendar.fromGregorian year month day & Calendar.dayOfWeek
  where 
    date = Clock.getCurrentTime >>= return . Calendar.toGregorian . Clock.utctDay