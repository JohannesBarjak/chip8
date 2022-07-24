{-# LANGUAGE GADTs, FlexibleInstances, RecordWildCards #-}
module Backend.IO (Cpu(..), indexGfx, initCpu) where

import CPU
import System.Random (randomIO)
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
    }

initCpu :: V.Vector Word8 -> IO Cpu
initCpu rom = do
    gfx    <- blankGfx
    i      <- newIORef 0
    memory <- V.thaw $ font <> V.replicate 0x1B0 0 <> rom <> V.replicate (0xE00 - V.length rom) 0
    pc     <- newIORef 0x200
    stack  <- newIORef []
    v      <- M.replicate 16 0
    keypad <- M.replicate 16 False
    dt     <- newIORef 0
    st     <- newIORef 0

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

    rand = randomIO

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

    (Gfx x y) %= f = do
        gfx' <- gfx <$> ask
        M.modify gfx' f (indexGfx x y)
    I  %= f = flip modifyIORef f . i =<< ask
    (Memory x) %= f = do
        mem <- memory <$> ask
        M.modify mem f x

    Pc %= f = flip modifyIORef f . pc =<< ask
    (V x) %= f = do
        v' <- v <$> ask
        M.modify v' f x
    (Keypad x) %= f = do
        kpd <- keypad <$> ask
        M.modify kpd f x

    Dt %= f = flip modifyIORef f . dt =<< ask
    St %= f = flip modifyIORef f . st =<< ask

    (Gfx x y) .= p = do
        gfx' <- gfx <$> ask
        M.write gfx' (indexGfx x y) p
    I .= a = flip writeIORef a . i =<< ask
    (Memory x) .= b = do
        mem <- memory <$> ask
        M.write mem x b

    Pc .= a = flip writeIORef a . pc =<< ask
    (V x) .= b = do
        v' <- v <$> ask
        M.write v' x b
    (Keypad x) .= k = do
        kpd <- keypad <$> ask
        M.write kpd x k
    
    Dt .= a = flip writeIORef a . dt =<< ask
    St .= a = flip writeIORef a . st =<< ask

indexGfx :: Num a => a -> a -> a
indexGfx x y = (x * 32) + y

blankGfx :: IO (IOVector Bool)
blankGfx = M.replicate (64 * 64) False
