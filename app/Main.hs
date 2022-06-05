{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Relude.Unsafe as Unsafe (head)

import CPU
import Emulator

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as G

import Control.Lens
import Numeric (readHex)
import System.Random (initStdGen)

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
        60
        (initCpu rom sd)
        (`displayScreen` 10)
        getKeyboardInput
        (execState . runEmulator)

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
        setKey b      = maybe id (setKeypad b . key) (keyMap k)
        setKeypad b n = set (keypad.ix n) b
        key n         = let [(go,_)] = readHex [n] in go
getKeyboardInput _ = id

keyMap :: Char -> Maybe Char
keyMap '1' = Just '1'
keyMap '2' = Just '2'
keyMap '3' = Just '3'
keyMap '4' = Just 'c'
keyMap 'q' = Just '4'
keyMap 'w' = Just '5'
keyMap 'e' = Just '6'
keyMap 'r' = Just 'd'
keyMap 'a' = Just '7'
keyMap 's' = Just '8'
keyMap 'd' = Just '9'
keyMap 'f' = Just 'e'
keyMap 'z' = Just 'a'
keyMap 'x' = Just '0'
keyMap 'c' = Just 'b'
keyMap 'v' = Just 'f'
keyMap  _  = Nothing

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
