{-# LANGUAGE FlexibleContexts #-}
module Emulator (emulatorCycle) where

import CPU

import Data.Bits
import Relude.Extra (bimapBoth)
import Data.List (elemIndex)

-- Decrement timers and run the necessary number of instructions per cycle
emulatorCycle :: MonadEmulator m => Int -> CompatMode -> m ()
emulatorCycle ipc mode = do
    traverse_ (liftA2 whenM (fmap (> 0) . look) (-= 1)) [Dt, St]
    replicateM_ ipc (runInstruction mode)

-- Fetch and Execute an instruction
runInstruction :: MonadEmulator m => CompatMode -> m ()
runInstruction mode = do
    pc <- look Pc
    incPc
    
    b0 <- rawMem pc
    b1 <- rawMem (pc + 1)
    eval $ toInstruction b0 b1 mode

-- Eval instruction
eval :: MonadEmulator m => Instruction -> m ()
eval ClearScreen = clearGfx
eval Return      = jmp =<< pop

eval (Jmp  addr) = jmp addr
eval (Call addr) = do
    push =<< look Pc
    jmp addr

eval (SkipEq    x (NN nn)) = skipIf . (== nn) =<< look (V x)
eval (SkipEq    x (VI  y)) = skipIf =<< liftR2 (==) (V x) (V y)
eval (SkipNotEq x (NN nn)) = skipIf . (/= nn) =<< look (V x)
eval (SkipNotEq x (VI  y)) = skipIf =<< liftR2 (/=) (V x) (V y)

eval (Set x (NN nn)) = V x .= nn
eval (Set x (VI  y)) = V x =: V y
eval (Add x (NN nn)) = V x += nn

eval (Add x (VI  y)) = do
    (vx, vy) <- look2 (V x) (V y)
    V x += vy
    vf .= bool 0 1 (vy > (255 - vx))

eval (Sub x  y) = do
    (vx, vy) <- look2 (V x) (V y)
    V x -= vy
    vf .= bool 1 0 (vy > vx)

eval (SubN x y) = do
    (vx, vy) <- look2 (V x) (V y)
    V x .= vy - vx
    vf .= bool 1 0 (vx > vy)

eval (ShiftRight x my) = do
    -- When in cosmic vip mode assign to vx
    whenJust my $ \y -> V x =: V y

    vx <- look (V x)
    V x .= vx `shiftR` 1
    vf .= vx .&. 1

eval (ShiftLeft  x my) = do
    -- When in cosmic vip mode assign to vx
    whenJust my $ \y -> V x =: V y

    vx <- look (V x)
    V x .= vx `shiftL` 1
    vf .= vx `shiftR` 7

eval (Or  x y) = V x <~ liftR2 (.|.) (V x) (V y)
eval (And x y) = V x <~ liftR2 (.&.) (V x) (V y)
eval (Xor x y) = V x <~ liftR2  xor  (V x) (V y)

eval (Index addr) = I .= addr

eval (JmpOff mx addr) = do
    vx <- look . V $ fromMaybe 0 mx
    jmp (addr + fromIntegral vx)

eval (Rand  x nn) = V x <~ fmap (.&. nn) rand

eval (Draw x y n) = do
    vf .= 0

    (vx, vy) <- bimapBoth fromIntegral <$> look2 (V x) (V y)

    -- draws a sprite by getting the
    -- nth sprite row and xoring the row's pixels
    -- at the appropriate location
    for_ [0..n - 1] $ \dy -> do
        let yPos = vy + dy
        row <- spriteRow <$> look (Memory dy)

        for_ (zip row [vx..vx + 7]) $ \(write, xPos) -> do
            pixel <- look (Gfx xPos yPos)
            Gfx xPos yPos .= xor write pixel

            -- collision detection
            setVFIf (write && pixel)

    where spriteRow byte = [toBool $ (byte `shiftR` idx) .&. 1 | idx <- [7,6..0]]
          toBool = (/= 0)

eval (SkipKey    x) = do
    vx <- fromIntegral <$> look (V x)
    skipIf =<< look (Keypad vx)

eval (SkipNotKey x) = do
    vx <- fromIntegral <$> look (V x)
    skipIf . not =<< look (Keypad vx)

eval (GetDelay x) = V x =: Dt
eval (SetDelay x) = Dt =: V x
eval (Sound    x) = St =: V x

eval (AddIndex x) = do
    vx <- look (V x)
    I += fromIntegral vx
    setVFIf . (> 0xFFF) =<< look I

eval (Font     x) = do
    vx <- look (V x)
    I .= fromIntegral (vx .&. 0xF) * 5

eval (GetKey  x) = do
    keys <- traverse (look . Keypad) [0..15]
    maybe (Pc -= 2) ((V x .=) . fromIntegral) (elemIndex True keys)

eval (BCDConv x) = do
    vx <- look (V x)
    traverse_ (liftA2 (.=) Memory $ calcDigit vx) [0..2]

    where calcDigit vx n = (vx `quot` (100 `quot` 10^n)) `rem` 10

eval (Slice a x m) = do
    idx <- look I
    for_ [0..x] $ case a of
        Write -> liftA2 (=:) Memory V
        Read  -> liftA2 (=:) V Memory

    when (m == CosmicVip) $ do
        I .= (idx + x + 1)

liftR2 :: MonadEmulator m => (a -> b -> c) -> Ref a -> Ref b -> m c
liftR2 f r1 r2 = uncurry f <$> look2 r1 r2

look2 :: MonadEmulator m => Ref a -> Ref b -> m (a, b)
look2 r1 r2 = liftA2 (,) (look r1) (look r2)

jmp :: MonadEmulator m => Int -> m ()
jmp = (Pc .=)

skipIf :: MonadEmulator m => Bool -> m ()
skipIf b = when b incPc

setVFIf :: MonadEmulator m => Bool -> m ()
setVFIf b = when b (vf .= 1)

incPc :: MonadEmulator m => m ()
incPc = Pc += 2

vf :: Ref Word8
vf = V 0xF
