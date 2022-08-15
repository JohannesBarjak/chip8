{-# LANGUAGE FlexibleContexts #-}
module Emulator (emulatorCycle) where

import CPU

import Data.Bits
import Relude.Extra (bimapBoth)
import Data.List (elemIndex)
import Data.Traversable (for)

emulatorCycle :: MonadEmulator m => Int -> m ()
emulatorCycle ipc = do
    traverse_ (liftA2 whenM (fmap (> 0) . look) (-= 1)) [Dt, St]
    replicateM_ ipc runInstruction

runInstruction :: MonadEmulator m => m ()
runInstruction = do
    curPc <- look Pc
    incPc
    
    (b0, b1) <- look2 (Memory curPc) (Memory $ curPc + 1)
    eval $ toInstruction b0 b1

eval :: MonadEmulator m => Instruction -> m ()
eval ClearScreen = clearGfx
eval Return = jmp =<< pop
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

eval (ShiftRight x _) = do
    vx <- look (V x)
    V x .= vx `shiftR` 1
    vf .= vx .&. 1
eval (ShiftLeft  x _) = do
    vx <- look (V x)
    V x .= vx `shiftL` 1
    vf .= vx `shiftR` 7

eval (Or  x y) = V x <~ liftR2 (.|.) (V x) (V y)
eval (And x y) = V x <~ liftR2 (.&.) (V x) (V y)
eval (Xor x y) = V x <~ liftR2  xor  (V x) (V y)

eval (Index addr) = I .= addr
eval (JmpOff _ addr) = do
    vx <- look (V 0)
    jmp (addr + fromIntegral vx)
eval (Rand  x nn) = V x <~ fmap (.&. nn) rand

eval (Draw x y n) = do
    vf .= 0

    (vx, vy) <- bimapBoth fromIntegral <$> look2 (V x) (V y)
    idx <- look I

    chunk <- for [0..n - 1] $ \dy -> do
        let yPos = vy + dy
        sprite <- toSprite <$> look (Memory (idx + dy))
        pixels <- traverse (look . flip Gfx yPos) [vx..vx + 7]

        pure $ zip sprite pixels

    setVFIf $ any (any $ uncurry (&&)) chunk
    draw chunk vx vy

    where toSprite sprite = [toBool $ (sprite `shiftR` idx) .&. 1 | idx <- [7,6..0]]
          draw chunk vx vy =
                for_ (zip chunk [vy..]) $ \(buf, yPos) ->
                    for_ (zip buf [vx..]) $ \((write, pixel), xPos) ->
                        Gfx xPos yPos .= (write /= pixel)

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
    (vx, idx) <- look2 (V x) I
    traverse_ ((.=) . Memory . (+ idx) <*> calcDigit vx) [0..2]

    where calcDigit vx n = (vx `quot` (100 `quot` 10^n)) `rem` 10

eval (WriteMemory x) = do
    idx <- look I
    traverse_ ((=:) . Memory . (+ idx) <*> V) [0..x]
eval (ReadMemory  x) = do
    idx <- look I
    traverse_ ((=:) . V <*> Memory . (+ idx)) [0..x]

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

toBool :: Word8 -> Bool
toBool 0 = False
toBool _ = True
