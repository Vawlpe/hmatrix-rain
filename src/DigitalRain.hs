module DigitalRain (Drop(..), Color(..), rain, randomDrop) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM)
import System.Random
import Data.Char
import Data.Maybe

data Drop = Drop {
    x :: Int,
    y :: Int,
    char :: Char,
    color :: Color
}

instance Show Drop where
    show drop = concat [
        "\ESC[",show (y drop), ";", show (x drop), "H",
        "\ESC[", show (color drop), "m",
        [char drop],
        "\ESC[0m"
        ]

data Color
    = Red
    | Green
    | Blue
    | Yellow
    | White
    deriving (Eq, Enum)

instance Show Color where
    show Red = "\ESC[31m"
    show Green = "\ESC[32m"
    show Blue = "\ESC[34m"
    show Yellow = "\ESC[33m"
    show White = "\ESC[37m"

rain :: [IO Drop] -> IO ()
rain drops = case drops of
    [] -> do
        rain [randomDrop]
    _ -> do
        drops' <- return $ moveDrops drops
        putStr $ unlines $ show
        randDelay <- randomRIO (100, 10000)
        threadDelay randDelay
        rain drops'

randomDrop :: IO Drop
randomDrop = do
    x <- randomRIO (0, 80)
    y <- return 0
    char <- randomRIO (' ', '~')
    color <- toEnum <$> randomRIO (0, 4)
    return $ Drop x y char color

moveDrop :: IO Drop -> IO Drop
moveDrop drop = do
    drop' <- drop
    y' <- return $ y drop' + 1
    if y' > 24
        then do randomDrop
        else do return $ drop' { y = y' }

moveDrops :: [IO Drop] -> [IO Drop]
moveDrops drops =  moveDrop <$> drops