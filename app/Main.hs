{-# LANGUAGE OverloadedLists #-}
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

import CPU

data Emulator m = Emulator (CpuState m) (Int, Int)

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

    case args !!? 1 of
        Just "io" -> do
            cpu <- B.IO.initial rom sd
            runChip8 cpu
        _ -> do
            let cpu = B.St.initial rom sd
            runChip8 cpu

getRom :: MonadIO m => FilePath -> m (Maybe ByteString)
getRom filename = do
    rom <- readFileBS filename
    pure $ if BS.length rom < 4096 - 512
        then Just rom
        else Nothing

runChip8 :: MonadEmulator m => CpuState m -> IO ()
runChip8 cpu =
    playIO
        (InWindow
                "Chip8"
                winSize
                (10, 10))
        white
        fps
        (Emulator cpu winSize)
        draw
        eventHandler
        (const $ runCycle ipc)

        where fps = 60
              ipc = 500 `quot` fps

runCycle :: (MonadEmulator m, MonadIO n) => Int -> Emulator m -> n (Emulator m)
runCycle ipc (Emulator cpu wSize) = do
    (_, cpu') <- runIO (emulatorCycle ipc) cpu
    pure (Emulator cpu' wSize)

draw :: (MonadEmulator m, MonadIO n) => Emulator m -> n Picture
draw (Emulator cpu wSize) = fst <$> runIO (displayScreen wSize) cpu

eventHandler :: (MonadEmulator m, MonadIO n) => Event -> Emulator m -> n (Emulator m)
eventHandler e (Emulator cpu wSize) = do
    (mSize, cpu') <- runIO (eventHandler' e) cpu
    pure $ Emulator cpu' (fromMaybe wSize mSize)

eventHandler' :: MonadEmulator m => Event -> m (Maybe (Int, Int))
eventHandler' (EventKey (Char k) pressed _ _) = do
    whenJust (lookup k keyMap) $ \x -> Keypad x .= (pressed /= Up)
    pure Nothing
eventHandler' (EventResize wSize) = pure $ Just wSize

eventHandler' _ = pure Nothing

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

displayScreen :: MonadEmulator m => (Int, Int) -> m Picture
displayScreen (w, h) = fmap pictures . sequence $ do
    x <- [0..63]
    let xOffset = (width - size * 64) / 2
    let xPos = fromIntegral x * size - width / 2
    y <- [0..31]
    let yOffset = (height - size * 32) / 2
    let yPos = fromIntegral (y + 1) * (-size) + height / 2

    pure $ pixelImage (xPos + xOffset) (yPos - yOffset) size <$> look (Gfx x y)
    where size = pixelSize width height
          width  = fromIntegral w
          height = fromIntegral h

pixelSize :: (Ord a, Fractional a) => a -> a -> a
pixelSize width height = min (width / 64) (height / 32)

pixelImage :: Float -> Float -> Float -> Bool -> Picture
pixelImage x y s p = Color (pixelColor p) square
    where pixelColor = bool black white
          square = Polygon [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
