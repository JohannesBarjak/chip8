{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module CPU
    ( MonadEmulator(..)
    , Ref(..)
    , Instruction(..)
    , Source(..)
    , CompatMode(..)
    , RW(..)
    , (<~), (=:), (+=), (-=)
    , toInstruction
    ) where

import Data.Bits
import Numeric (showHex)

import Prelude as P
import Relude.Extra (bimapBoth)

-- Cpu Instruction Set
data Instruction
    = ClearScreen
    | Return
    | Jmp Int
    | Call Int
    | SkipEq Int Source
    | SkipNotEq Int Source
    | SkipKey Int
    | SkipNotKey Int
    | Set Int Source
    | Sub Int Int
    | SubN Int Int
    | Add Int Source
    | ShiftRight Int (Maybe Int)
    | ShiftLeft Int (Maybe Int)
    | Or Int Int
    | And Int Int
    | Xor Int Int
    | Index Int
    | JmpOff (Maybe Int) Int
    | Rand Int Word8
    | Draw Int Int Int
    | GetDelay Int
    | SetDelay Int
    | Sound Int
    | AddIndex Int
    | Font Int
    | GetKey Int
    | BCDConv Int
    | Slice RW Int CompatMode

data RW = Read | Write

data Source     = VI Int | NN Word8
data CompatMode = Chip48 | CosmicVip deriving Eq

-- Accessor GADTs to provide a uniform api for
-- mutation and access
data Ref a where
    Gfx    :: Int -> Int -> Ref Bool
    I      :: Ref Int
    Memory :: Int -> Ref Word8
    Pc     :: Ref Int
    V      :: Int -> Ref Word8
    Keypad :: Int -> Ref Bool
    Dt     :: Ref Word8
    St     :: Ref Word8

class Monad m => MonadEmulator m where
    data CpuState m

    runIO    :: MonadIO n => m a -> CpuState m -> n (a, CpuState m)

    look     :: Ref a -> m a
    push     :: Int -> m ()
    pop      :: m Int
    rand     :: m Word8
    clearGfx :: m ()
    rawMem   :: Int -> m Word8
    (%=)     :: Ref a -> (a -> a) -> m ()
    (.=)     :: Ref a -> a -> m ()

    infix 4 %=, .=

-- Infix functions insipired by the lens library
infix 4 <~, =:, +=, -=

(<~) :: MonadEmulator m => Ref a -> m a -> m ()
ref <~ ma  = (ref .=) =<< ma

(=:) :: MonadEmulator m => Ref a -> Ref a -> m ()
r1  =: r2  = (r1 .=) =<< look r2

(+=) :: (MonadEmulator m, Num a) => Ref a -> a -> m ()
ref += val = ref %= (+val)

(-=) :: (MonadEmulator m, Num a) => Ref a -> a -> m ()
ref -= val = ref %= subtract val

toInstruction :: Word8 -> Word8 -> CompatMode -> Instruction
toInstruction b0 b1 = toInstruction' instr
    where instr = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1

toInstruction' :: Int -> CompatMode -> Instruction
toInstruction' instr mode = case upper of
        0x0 -> case (x,y,lower) of
            (0x0, 0xE, 0x0) -> ClearScreen
            (0x0, 0xE, 0xE) -> Return
            _ -> invalidInstruction

        0x1 -> Jmp  nnn
        0x2 -> Call nnn
        0x3 -> SkipEq    x (NN nn)
        0x4 -> SkipNotEq x (NN nn)
        0x5 -> SkipEq    x (VI  y)
        0x6 -> Set x (NN nn)
        0x7 -> Add x (NN nn)

        0x8 -> case lower of
            0x0 -> Set x (VI y)
            0x1 -> Or  x y
            0x2 -> And x y
            0x3 -> Xor x y
            0x4 -> Add x (VI y)
            0x5 -> Sub x y
            0x6 -> case mode of
                Chip48    -> ShiftRight x Nothing
                CosmicVip -> ShiftRight x (Just y)
            0x7 -> SubN x y
            0xE -> case mode of
                Chip48    -> ShiftLeft x Nothing
                CosmicVip -> ShiftLeft x (Just y)
            _ -> invalidInstruction

        0x9 | lower == 0 -> SkipNotEq x (VI y)
        0xA -> Index    nnn
        0xB -> case mode of
            Chip48    -> JmpOff (Just x) nnn
            CosmicVip -> JmpOff Nothing  nnn

        0xC -> Rand x nn
        0xD -> Draw x y lower
        0xE -> case (y,lower) of
            (0x9, 0xE) -> SkipKey    x
            (0xA, 0x1) -> SkipNotKey x
            _ -> invalidInstruction

        0xF -> case (y,lower) of
            (0x0, 0x7) -> GetDelay x
            (0x1, 0x5) -> SetDelay x
            (0x1, 0x8) -> Sound    x
            (0x1, 0xE) -> AddIndex x
            (0x2, 0x9) -> Font     x
            (0x0, 0xA) -> GetKey   x
            (0x3, 0x3) -> BCDConv  x
            (0x5, 0x5) -> Slice Write x mode
            (0x6, 0x5) -> Slice Read  x mode
            _ -> invalidInstruction
        _ -> invalidInstruction

    where nnn   = instr .&. 0x0FFF
          nn    = fromIntegral $ instr .&. 0xFF
          (x,y) = bimapBoth (.&. 0xF) (instr `shiftR` 8, instr `shiftR` 4)
          upper = instr `shiftR` 12
          lower = instr .&. 0xF

          invalidInstruction = error $ "CPU: unsupported instruction: " <> invalid
          invalid = foldMap (toText . flip showHex "") [upper,x,y,lower]
