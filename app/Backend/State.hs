{-# LANGUAGE TemplateHaskell, FlexibleInstances, GADTs #-}
module Backend.State where

import Prelude as P
import System.Random (StdGen, Random (random))

import CPU
import Font
import Control.Lens as L

import Data.Vector as V
import Data.ByteString qualified as BS

data Cpu = CPU
    { _gfx    :: Vector (Vector Bool)
    , _i      :: Int
    , _memory :: Vector Word8
    , _pc     :: Int
    , _stack  :: [Int]
    , _v      :: Vector Word8
    , _keypad :: Vector Bool
    , _dt     :: Word8
    , _st     :: Word8
    , _seed   :: StdGen
    }

makeLenses ''Cpu

initCpu :: Vector Word8 -> StdGen -> Cpu
initCpu rom sd = CPU
    { _gfx    = blankScreen
    , _i      = 0
    , _memory = font <> V.replicate 0x1B0 0 <> rom <> V.replicate (0xE00 - V.length rom) 0
    , _pc     = 0x200
    , _stack  = []
    , _v      = V.replicate 16 0
    , _keypad = V.replicate 16 False
    , _dt     = 0
    , _st     = 0
    , _seed   = sd
    }

instance MonadEmulator (State Cpu) where
    look (Gfx x  y) = fromMaybe False . (^?gfx.ix x.ix y) <$> get
    look I          = use i
    look (Memory x) = do
        cpu <- get
        pure (cpu^?!memory.ix x)
    look Pc         = use pc
    look (V x)      = do
        cpu <- get
        pure (cpu^?!v.ix x)
    look (Keypad x) = do
        cpu <- get
        pure (cpu^?!keypad.ix x)
    look Dt         = use dt
    look St         = use st

    rand = zoom seed $ state random

    push x = stack L.%= (x:)
    pop    = zoom stack . state $ fromMaybe (error "CPU: empty stack") . P.uncons

    clearGfx = gfx L..= blankScreen

    (%=) (Gfx x  y) = (gfx.ix x.ix y L.%=)
    (%=) I          = (i L.%=)
    (%=) (Memory x) = (memory.ix x L.%=)
    (%=) Pc         = (pc L.%=)
    (%=) (V x)      = (v.ix x L.%=)
    (%=) (Keypad x) = (keypad.ix x L.%=)
    (%=) Dt         = (dt L.%=)
    (%=) St         = (st L.%=)

blankScreen :: Vector (Vector Bool)
blankScreen = V.replicate 64 $ V.replicate 32 False

indexScreen :: Vector (Vector a) -> Int -> Int -> a
indexScreen screen x = (V.!) $ screen V.! x

toMemory :: ByteString -> Vector Word8
toMemory bs = V.fromList $ BS.unpack bs
