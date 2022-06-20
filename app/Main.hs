{-# LANGUAGE NamedFieldPuns, OverloadedLists #-}
module Main where

import Relude.Unsafe as Unsafe (head)

import CPU
import Emulator

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as G

import Control.Lens
import System.Random (initStdGen)
import Data.Map.Strict (lookup)

winWidth, winHeight :: Int
winWidth  = 640
winHeight = 320

winSize :: (Int, Int)
winSize = (winWidth, winHeight)

main :: IO ()
main = do
    (Just rom) <- getRom
    sd <- initStdGen

    play
        (InWindow
               "Hello World"
                winSize
                (10, 10))
        (greyN 0.8)
        fps
        (initCpu rom sd)
        (`displayScreen` 10)
        getKeyboardInput
        (execState . const (runEmulator ipc))

        where fps = 60
              ipc = 500 `quot` 60

getRom :: IO (Maybe Memory)
getRom = do
    filename <- Unsafe.head <$> getArgs
    rom <- toMemory <$> readFileBS filename
    pure $ if length rom < 4096 - 512
        then Just rom
        else Nothing

getKeyboardInput :: Event -> Cpu -> Cpu
getKeyboardInput (EventKey (Char k) pressed _ _)
    | pressed == G.Down = setKey True
    | pressed == Up     = setKey False
    where
        setKey b      = maybe id (setKeypad b) (lookup k keyMap)
        setKeypad b n = set (keypad.ix n) b
getKeyboardInput _ = id

keyMap :: Map Char Int
keyMap =
    [ ('1', 0x1), ('2', 0x2)
    , ('3', 0x3), ('4', 0xC)
    , ('q', 0x4), ('Q', 0x4)
    , ('w', 0x5), ('W', 0x5)
    , ('e', 0x6), ('E', 0x6)
    , ('r', 0xD), ('R', 0xD)
    , ('a', 0x7), ('A', 0x7)
    , ('s', 0x8), ('S', 0x8)
    , ('d', 0x9), ('D', 0x9)
    , ('f', 0xE), ('F', 0xE)
    , ('z', 0xA), ('Z', 0xA)
    , ('x', 0x0), ('X', 0x0)
    , ('c', 0xB), ('C', 0xB)
    , ('v', 0xF), ('V', 0xF)
    ]

displayScreen :: Cpu -> Int -> Picture
displayScreen CPU{_gfx} sc = pictures $ do
    x <- [0..63]
    let xPos = x * sc - (winWidth `div` 2)
    y <- [0..31]
    let yPos = (y + 1) * (-sc) + (winHeight `div` 2)

    pure $ pixelImage (fromIntegral xPos) (fromIntegral yPos) sc (indexScreen _gfx x y)

pixelImage :: Float -> Float -> Int -> Bool -> Picture
pixelImage x y sc p = Color (pixelColor p) square
    where pixelColor True  = white
          pixelColor False = black
          s      = fromIntegral sc
          square = Polygon [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
