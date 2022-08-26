{-# LANGUAGE OverloadedLists, RecordWildCards #-}
module Main where

import Emulator

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (initStdGen)
import Data.Map.Strict (lookup)

import Backend.State as B.St
import Backend.IO as B.IO

import Graphics.Gloss.Interface.IO.Game as G (playIO, KeyState(..))
import Data.ByteString qualified as BS
import Data.Char (toUpper)

import CPU

data Emulator m = Emulator
    { cpu    :: CpuState m
    , wSize  :: (Int, Int)
    , paused :: Bool
    }

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

    let mode = bool Chip48 CosmicVip ("cosmic" `elem` args)

    if "io" `elem` args then
        do
            cpu <- B.IO.initial rom sd
            runChip8 cpu mode
        else do
            let cpu = B.St.initial rom sd
            runChip8 cpu mode

getRom :: MonadIO m => FilePath -> m (Maybe ByteString)
getRom filename = do
    rom <- readFileBS filename
    pure $ if BS.length rom < 4096 - 512
        then Just rom
        else Nothing

runChip8 :: MonadEmulator m => CpuState m -> CompatMode -> IO ()
runChip8 cpu mode =
    playIO
        (InWindow
                "Chip8"
                winSize
                (10, 10))
        black
        fps
        (Emulator cpu winSize False)
        draw
        eventHandler
        (const $ runCycle ipc mode)

        where fps = 60
              ipc = 500 `quot` fps

runCycle :: (MonadEmulator m, MonadIO n) => Int -> CompatMode -> Emulator m -> n (Emulator m)
runCycle ipc mode (Emulator cpu wSize False) = do
    (_, cpu') <- runIO (emulatorCycle ipc mode) cpu
    pure (Emulator cpu' wSize False)
runCycle _ _ e = pure e

draw :: (MonadEmulator m, MonadIO n) => Emulator m -> n Picture
draw (Emulator cpu wSize _) = fst <$> runIO (displayScreen wSize) cpu

eventHandler :: (MonadEmulator m, MonadIO n) => Event -> Emulator m -> n (Emulator m)
eventHandler e em@(Emulator cpu _ _) = do
    (Emulator _ wSize' p, cpu') <- runIO (eventHandler' e em) cpu
    pure $ Emulator cpu' wSize' p

eventHandler' :: MonadEmulator m => Event -> Emulator m -> m (Emulator m)
eventHandler' (EventKey (Char k) G.Down _ _) em@(Emulator {..})
    | toUpper k == 'P' = pure $ em { paused = not paused }
eventHandler' (EventKey (Char k) pressed _ _) e = do
    whenJust (lookup (toUpper k) keyMap) $ \x -> Keypad x .= (pressed /= Up)
    pure e
eventHandler' (EventResize wSize) (Emulator cpu _ p) = pure $ Emulator cpu wSize p

eventHandler' _ e = pure e

keyMap :: Map Char Int
keyMap =
    [ ('1', 0x1), ('2', 0x2)
    , ('3', 0x3), ('4', 0xC)
    , ('Q', 0x4), ('W', 0x5)
    , ('E', 0x6), ('R', 0xD)
    , ('A', 0x7), ('S', 0x8)
    , ('D', 0x9), ('F', 0xE)
    , ('Z', 0xA), ('X', 0x0)
    , ('C', 0xB), ('V', 0xF)
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
    where pixelColor = bool (greyN 0.1) (greyN 0.9)
          square = Polygon [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
