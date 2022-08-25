{-# LANGUAGE GADTs, RecordWildCards, TypeFamilies, GeneralisedNewtypeDeriving #-}
module Backend.IO (IOEmulator, initial) where

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

initial :: V.Vector Word8 -> StdGen -> IO (CpuState IOEmulator)
initial rom sd = do
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

    pure $ IOCpu Cpu {..}

newtype IOEmulator a = IOEmulator (ReaderT Cpu IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cpu, MonadFail)

instance MonadEmulator IOEmulator where
    newtype CpuState IOEmulator = IOCpu Cpu

    runIO (IOEmulator r) (IOCpu c) = do
        a <- liftIO $ runReaderT r c
        pure (a, IOCpu c)

    look ref = ask >>= \Cpu{..} ->
        liftIO $ case ref of
            (Gfx x  y) -> M.read gfx (indexGfx x y)
            I          -> readIORef i
            (Memory x) -> do
                i' <- readIORef i
                M.read memory (i' + x)
            Pc         -> readIORef pc
            (V x)      -> M.read v x
            (Keypad x) -> M.read keypad x
            Dt         -> readIORef dt
            St         -> readIORef st

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
        liftIO $ M.copy gfx' =<< blankGfx
    rawMem x = ask >>= \Cpu{..} -> liftIO $ M.read memory x

    r %= f = ask >>= \Cpu{..} ->
        liftIO $ case r of
            (Gfx x  y) -> M.modify gfx f (indexGfx x y)
            I          -> modifyIORef i f
            (Memory x) -> do
                i' <- readIORef i
                M.modify memory f (i' + x)

            Pc         -> modifyIORef pc f
            (V x)      -> M.modify v f x
            (Keypad x) -> M.modify keypad f x

            Dt         -> modifyIORef dt f
            St         -> modifyIORef st f

    r .= val = ask >>= \Cpu{..} ->
        liftIO $ case r of
            (Gfx x  y) -> M.write gfx (indexGfx x y) val
            I          -> writeIORef i val
            (Memory x) -> do
                i' <- readIORef i
                M.write memory (i' + x) val

            Pc         -> writeIORef pc val
            (V x)      -> M.write v x val
            (Keypad x) -> M.write keypad x val
    
            Dt         -> writeIORef dt val
            St         -> writeIORef st val

indexGfx :: Integral a => a -> a -> a
indexGfx x y = ((x `rem` 64) * 32) + (y `rem` 32)

blankGfx :: IO (IOVector Bool)
blankGfx = M.replicate (64 * 32) False
