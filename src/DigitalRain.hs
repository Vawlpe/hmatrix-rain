module DigitalRain (rain, randomDrop, Drop, show) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM)
import System.Random
import Data.Char
import Data.Maybe

data Drop = Drop {
	x :: Int,
	y :: Int,
	c :: Char,
	color :: Int
}

instance Show Drop where
	show drop  =  "\ESC["   ++ show (y drop)     ++ ";" ++ show (x drop) ++ "H"
			   ++ "\ESC[3"  ++ show (color drop) ++ "m" ++ [c drop]		 ++ "\ESC[0m"

randomDrop :: RandomGen g => g -> Int -> (g, Drop)
randomDrop g maxW  = (g'', Drop rx 0 rc rcol) where
	(rx, g')      = randomR (0,  maxW) g
	(rc, g'')     = randomR (' ', '~') g'
	(rcol, g''')  = randomR (1, 9) g''

moveDrop :: Int -> Int -> [Drop] -> [Drop]
moveDrop maxH maxW drops = filter (\d -> y d < maxH) $ map (\d -> d {y = y d + 1}) drops

rain :: RandomGen g => g -> Int -> Int -> [Drop] -> IO ()
rain g maxH maxW drops = do
	let (g', d)   = randomDrop g maxW
	let (g'', d2) = randomDrop g' maxW
	let (g''', d3) = randomDrop g'' maxW
	threadDelay   $ 20000
	let drops'    = moveDrop maxH maxW drops
	putStr $ unlines $ map show drops' ++ [show d] ++ [show d2]
	putStr "\ESC[2J"
	rain g''' maxH maxW (drops' ++ [d] ++ [d2])
