{-# LANGUAGE GADTs, FlexibleInstances, RecordWildCards #-}
module Backend.IO (Cpu(..), indexGfx, initCpu) where

import CPU
import System.Random (StdGen, random)
import Data.Vector.Mutable as M

import Font
import Data.Vector qualified as V

data Cpu = Cpu
    { gfx    :: IOVector Bool
    , i      :: IORef Int
    , memory :: IOVector Word8
    , pc     :: IORef Int
    , stack  :: IORef [Int]
    , v      :: IOVector Word8
    , keypad :: IOVector Bool
    , dt     :: IORef Word8
    , st     :: IORef Word8
    , seed   :: IORef StdGen
    }

initCpu :: V.Vector Word8 -> StdGen -> IO Cpu
initCpu rom sd = do
    gfx    <- blankGfx
    i      <- newIORef 0
    memory <- V.thaw $ font <> V.replicate 0x1B0 0 <> rom <> V.replicate (0xE00 - V.length rom) 0
    pc     <- newIORef 0x200
    stack  <- newIORef []
    v      <- M.replicate 16 0
    keypad <- M.replicate 16 False
    dt     <- newIORef 0
    st     <- newIORef 0
    seed   <- newIORef sd

    pure Cpu {..}

instance MonadEmulator (ReaderT Cpu IO) where
    look ref = do
        cpu <- ask
        case ref of
            (Gfx x  y) -> M.read (gfx cpu) (indexGfx x y)
            I          -> readIORef (i cpu)
            (Memory x) -> M.read (memory cpu) x
            Pc         -> readIORef (pc cpu)
            (V x)      -> M.read (v cpu) x
            (Keypad x) -> M.read (keypad cpu) x
            Dt         -> readIORef (dt cpu)
            St         -> readIORef (st cpu)

    rand = do
        sd <- seed <$> ask
        (v, sd') <- random <$> readIORef sd
        writeIORef sd sd'
        pure v

    push a = do
        s <- stack <$> ask
        modifyIORef s (a:)
    
    pop = do
        s <- stack <$> ask
        (x:xs) <- readIORef s
        writeIORef s xs
        pure x

    clearGfx = do
        gfx' <- gfx <$> ask
        M.copy gfx' =<< lift blankGfx

    (Gfx x y) %= f = modifyCpuMemory gfx f (indexGfx x y)
    I  %= f = flip modifyIORef f . i =<< ask
    (Memory x) %= f = modifyCpuMemory memory f x

    Pc %= f = flip modifyIORef f . pc =<< ask
    (V x) %= f = modifyCpuMemory v f x
    (Keypad x) %= f = modifyCpuMemory keypad f x

    Dt %= f = flip modifyIORef f . dt =<< ask
    St %= f = flip modifyIORef f . st =<< ask

    (Gfx x y) .= p = writeCpuMemory gfx p (indexGfx x y)
    I .= a = flip writeIORef a . i =<< ask
    (Memory x) .= b = writeCpuMemory memory b x

    Pc .= a = flip writeIORef a . pc =<< ask
    (V x) .= b = writeCpuMemory v b x
    (Keypad x) .= k = writeCpuMemory keypad k x
    
    Dt .= a = flip writeIORef a . dt =<< ask
    St .= a = flip writeIORef a . st =<< ask

modifyCpuMemory :: (Cpu -> IOVector a) -> (a -> a) -> Int -> ReaderT Cpu IO ()
modifyCpuMemory r f i = do
    m <- r <$> ask
    M.modify m f i

writeCpuMemory :: (Cpu -> IOVector a) -> a -> Int -> ReaderT Cpu IO ()
writeCpuMemory r a' i = do
    m <- r <$> ask
    M.write m i a'

indexGfx :: Integral a => a -> a -> a
indexGfx x y = ((x `rem` 64) * 32) + (y `rem` 32)

blankGfx :: IO (IOVector Bool)
blankGfx = M.replicate (64 * 32) False
