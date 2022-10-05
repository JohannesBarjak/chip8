{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
module Backend.State (StateEmulator, initial) where

import Prelude as P
import System.Random (StdGen, Random (random))

import CPU
import Font
import Control.Lens as L

import Data.Vector as V

data Cpu = Cpu
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

initial :: Vector Word8 -> StdGen -> CpuState StateEmulator
initial rom sd = StateCpu Cpu
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

newtype StateEmulator a = StateEmulator (State Cpu a)
    deriving (Functor, Applicative, Monad)

instance MonadEmulator StateEmulator where
    newtype CpuState StateEmulator = StateCpu Cpu

    runIO (StateEmulator s) (StateCpu c) = pure . fmap StateCpu $ runState s c

    look r = StateEmulator $ do
        cpu <- get
        case r of
            (Gfx x  y) -> fromMaybe False . (^?gfxWrapping x y) <$> get
            I          -> use i
            (Memory x) -> do
                i' <- use i
                pure $ cpu^?!memory.ix (i' + x)
            Pc         -> use pc
            (V x)      -> pure (cpu^?!v.ix x)
            (Keypad x) -> pure (cpu^?!keypad.ix x)
            Dt         -> use dt
            St         -> use st

    rand = StateEmulator $ zoom seed $ state random

    push x = StateEmulator $ stack L.%= (x:)
    pop    = StateEmulator $ zoom stack . state $
        fromMaybe (error "CPU: empty stack") . P.uncons

    clearGfx = StateEmulator $ gfx L..= blankScreen
    rawMem x = StateEmulator $ do
        cpu <- get
        pure $ cpu^?!memory.ix x

    r %= f = StateEmulator $ case r of
        (Gfx x  y) -> gfxWrapping x y L.%= f
        I          -> i L.%= f
        (Memory x) -> do
            i' <- use i
            memory.ix (i' + x) L.%= f
        Pc         -> pc L.%= f
        (V x)      -> v.ix x L.%= f
        (Keypad x) -> keypad.ix x L.%= f
        Dt         -> dt L.%= f
        St         -> st L.%= f

    r .= val = StateEmulator $ case r of
        (Gfx x  y) -> gfxWrapping x y L..= val
        I          -> i L..= val
        (Memory x) -> do
            i' <- use i
            memory.ix (i' + x) L..= val
        Pc         -> pc L..= val
        (V x)      -> v.ix x L..= val
        (Keypad x) -> keypad.ix x L..= val
        Dt         -> dt L..= val
        St         -> st L..= val

gfxWrapping :: Applicative f => Int -> Int -> (Bool -> f Bool) -> Cpu -> f Cpu
gfxWrapping x y = gfx.ix (x `rem` 64).ix (y `rem` 32)

blankScreen :: Vector (Vector Bool)
blankScreen = V.replicate 64 $ V.replicate 32 False
