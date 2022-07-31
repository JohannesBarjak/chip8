{-# LANGUAGE OverloadedLists, TypeApplications, RankNTypes #-}
module Main where

import Emulator

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (initStdGen)
import Data.Map.Strict (lookup)

import Backend.State as B.St
import Backend.IO as B.IO

import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.ByteString qualified as BS
import Relude.Extra (bimapBoth)

import CPU

winWidth, winHeight :: Int
winWidth  = 640
winHeight = 320

winSize :: (Int, Int)
winSize = (winWidth, winHeight)

main :: IO ()
main = do
    args <- getArgs
    let filename = maybe (error "Enter a rom file") head $ nonEmpty args

    rom <- flip fmap (getRom filename) $ \case
            Just rom -> fromList $ BS.unpack rom
            Nothing  -> error $ show filename <> ": Exceeds chip8's memory capacity"

    sd  <- initStdGen

    if (args !!? 2) == Just "s" then do
        let cpu = B.St.initial rom sd
        runChip8 (runIO @StateEmulator) cpu
        else do
        cpu <- B.IO.initial rom sd
        runChip8 (runIO @IOEmulator) cpu

runChip8 :: MonadEmulator m => (forall a. m a -> EmState m -> IO (a, EmState m)) -> EmState m -> IO ()
runChip8 run cpu =
    playIO
        (InWindow
                "Chip8"
                winSize
                (10, 10))
        (greyN 0.8)
        fps
        cpu
        (fmap fst . run (displayScreen 10))
        (\e -> fmap snd . run (getKeyboardInput e))
        (const $ fmap snd . run (runEmulator ipc))

        where fps = 60
              ipc = 500 `quot` fps

getRom :: MonadIO m => FilePath -> m (Maybe ByteString)
getRom filename = do
    rom <- readFileBS filename
    pure $ if BS.length rom < 4096 - 512
        then Just rom
        else Nothing

getKeyboardInput :: MonadEmulator m => Event -> m ()
getKeyboardInput (EventKey (Char k) pressed _ _) = setKey (pressed /= Up)
    where setKey b = whenJust (lookup k keyMap) $ \x -> Keypad x .= b

getKeyboardInput _ = pure ()

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

displayScreen :: MonadEmulator m => Int -> m Picture
displayScreen sc = fmap pictures . sequence $ do
    x <- [0..63]
    let xPos = x * sc - (winWidth `div` 2)
    y <- [0..31]
    let yPos = (y + 1) * (-sc) + (winHeight `div` 2)

    pure $ pixelImage xPos yPos sc <$> look (Gfx x y)

pixelImage :: Int -> Int -> Int -> Bool -> Picture
pixelImage x y s p = Color (pixelColor p) square
    where pixelColor = bool black white
          square = Polygon $ map (bimapBoth fromIntegral)
            [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
