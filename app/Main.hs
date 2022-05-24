module Main where

import DigitalRain      (rain, randomDrop, Drop)
import System.Random	(getStdGen)
import Data.List

main :: IO ()
main = do
  putStr "\ESC[32m"
  gen <- getStdGen
  rain gen 30 150 $ []
  putStr "\ESC[0m"
