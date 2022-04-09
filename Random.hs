module Random where

import System.Random as Random

randomDev :: StdGen -> [String] -> (Int, StdGen)
randomDev gen devs =
  randomR (0, length devs - 1) gen