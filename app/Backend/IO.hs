{-# LANGUAGE GADTs, FlexibleInstances #-}
module Backend.IO where

import CPU
import System.Random (randomIO)
import Data.Vector.Mutable as M

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

instance MonadEmulator (ReaderT Cpu IO) where
    look (Gfx x y) = do
        gfx' <- gfx <$> ask
        M.read gfx' (indexGfx x y)

    look I  = readIORef . i =<< ask
    look (Memory x) = do
        mem <- memory <$> ask
        M.read mem x

    look Pc = readIORef . pc =<< ask
    look (V x) = do
        v' <- v <$> ask
        M.read v' x
    look (Keypad x) = do
        kpd <- keypad <$> ask
        M.read kpd x

    look Dt = readIORef . dt =<< ask
    look St = readIORef . st =<< ask

    rand = randomIO

    push a = do
        s <- stack <$> ask
        modifyIORef s (a:)
    
    pop = do
        s <- stack <$> ask
        (x:xs) <- readIORef s
        writeIORef s xs
        pure x

    clrGfx = do
        gfx' <- gfx <$> ask
        M.copy gfx' =<< blankGfx

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

indexGfx :: Num a => a -> a -> a
indexGfx x y = (y * 32) + x

blankGfx :: ReaderT Cpu IO (IOVector Bool)
blankGfx = M.replicate (64 * 32) False
