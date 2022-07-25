{-# LANGUAGE NamedFieldPuns, OverloadedLists #-}
module Main where

import Relude.Unsafe as Unsafe

import Emulator

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as G

import System.Random (initStdGen)
import Data.Map.Strict (lookup)

import Backend.IO

import Data.Vector.Mutable qualified as M
import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.ByteString qualified as BS

winWidth, winHeight :: Int
winWidth  = 640
winHeight = 320

winSize :: (Int, Int)
winSize = (winWidth, winHeight)

main :: IO ()
main = do
    (Just rom) <- fmap (fromList . BS.unpack) <$> getRom
    _ <- initStdGen
    cpu <- initCpu rom

    playIO
        (InWindow
               "Hello World"
                winSize
                (10, 10))
        (greyN 0.8)
        fps
        cpu
        (`displayScreen` 10)
        getKeyboardInput
        (\_ _ -> runReaderT (runEmulator ipc) cpu >> pure cpu)

        where fps = 60
              ipc = 500 `quot` 60

getRom :: IO (Maybe ByteString)
getRom = do
    filename <- Unsafe.head <$> getArgs
    rom <- readFileBS filename
    pure $ if BS.length rom < 4096 - 512
        then Just rom
        else Nothing

getKeyboardInput :: Event -> Cpu -> IO Cpu
getKeyboardInput (EventKey (Char k) pressed _ _)
    | pressed == G.Down = setKey True
    | pressed == Up     = setKey False
    where
        setKey b = maybe pure (setKeypad b) (lookup k keyMap)

        setKeypad :: Bool -> Int -> Cpu -> IO Cpu
        setKeypad b n cpu@Cpu{keypad} = do
            M.write keypad n b
            pure cpu

getKeyboardInput _ = pure

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

displayScreen :: Cpu -> Int -> IO Picture
displayScreen Cpu{gfx} sc = fmap pictures . sequence $ do
    x <- [0..63]
    let xPos = x * sc - (winWidth `div` 2)
    y <- [0..31]
    let yPos = (y + 1) * (-sc) + (winHeight `div` 2)

    pure $ pixelImage (fromIntegral xPos) (fromIntegral yPos) sc <$> M.read gfx (indexGfx x y)

pixelImage :: Float -> Float -> Int -> Bool -> Picture
pixelImage x y sc p = Color (pixelColor p) square
    where pixelColor True  = white
          pixelColor False = black
          s      = fromIntegral sc
          square = Polygon [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
