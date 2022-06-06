{-# LANGUAGE TemplateHaskell, GADTs, FlexibleInstances #-}
module CPU where

import Data.Bits
import Control.Lens hiding ((+=), (%=), (.=), (#=))
import Control.Lens qualified as L
import Numeric (showHex)

import Prelude as P

import Data.Vector (Vector)
import System.Random (StdGen, Random (random))

type Screen = Vector (Vector Bool)
type Memory = Vector Word8
type Reg    = Vector Word8
type KeyPad = Vector Bool

data Cpu = CPU
    { _gfx    :: Screen
    , _i      :: Int
    , _memory :: !Memory
    , _pc     :: Int
    , _stack  :: [Int]
    , _v      :: !Reg
    , _keypad :: KeyPad
    , _dt     :: Word8
    , _st     :: Word8
    , _seed   :: StdGen
    }

type Nibbles = (Word8, Word8)

data Instruction
    = ClearScreen
    | Return
    | Jmp Int
    | Call Int
    | SkipEq Int Target
    | SkipNotEq Int Target
    | SkipKey Int
    | SkipNotKey Int
    | Set Int Target
    | Sub Int Int
    | SubN Int Int
    | Add Int Target
    | ShiftRight Int Int
    | ShiftLeft Int Int
    | Or Int Int
    | And Int Int
    | Xor Int Int
    | Index Int
    | Rand Int Word8
    | Draw Int Int Int
    | GetDelay Int
    | SetDelay Int
    | Sound Int
    | AddIndex Int
    | Font Int
    | GetKey Int
    | BCDConv Int
    | WriteMemory Int
    | ReadMemory Int

data Target = VI Int | NN Word8

makeLenses ''Cpu

data Ref a where
    Gfx    :: Int -> Int -> Ref Bool
    Screen :: Ref Screen
    I      :: Ref Int
    Memory :: Int -> Ref Word8
    Pc     :: Ref Int
    V      :: Int -> Ref Word8
    Keypad :: Int -> Ref Bool
    Dt     :: Ref Word8
    St     :: Ref Word8

data Cmd a where
    Push :: Int -> Cmd ()
    Pop  :: Cmd Int

class Monad m => MonadEmulator m where
    look :: Ref a -> m a
    cmd  :: Cmd a -> m a
    rand :: m Word8
    (.=) :: Ref a -> a -> m ()
    (%=) :: Ref a -> (a -> a) -> m ()
    (<~) :: Ref a -> m a -> m ()
    (+=) :: Num a => Ref a -> a -> m ()
    (-=) :: Num a => Ref a -> a -> m ()
    (=:) :: Ref a -> Ref a -> m ()

    ref <~ ma  = (ref .=) =<< ma
    ref .= val = ref %= const val
    ref += val = ref %= (+val)
    ref -= val = ref %= subtract val
    r1  =: r2  = (r1 .=) =<< look r2

    infix 4 .=, %=, <~, +=, =:

instance MonadEmulator (State Cpu) where
    look (Gfx x  y) = fromMaybe False . (^?gfx.ix x.ix y) <$> get
    look Screen     = use gfx
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

    cmd (Push x) = stack L.%= (x:)
    cmd Pop      = zoom stack . state $ fromMaybe (error "CPU: empty stack") . P.uncons

    (%=) (Gfx x  y) = (gfx.ix x.ix y L.%=)
    (%=) Screen     = (gfx L.%=)
    (%=) I          = (i L.%=)
    (%=) (Memory x) = ((memory.ix x) L.%=)
    (%=) Pc         = (pc L.%=)
    (%=) (V x)      = (v.ix x L.%=)
    (%=) (Keypad x) = (keypad.ix x L.%=)
    (%=) Dt         = (dt L.%=)
    (%=) St         = (st L.%=)

toInstruction :: Nibbles -> Nibbles -> Instruction
toInstruction (0x0, 0x0) (0xE, 0x0) = ClearScreen
toInstruction (0x0, 0x0) (0xE, 0xE) = Return
toInstruction (0x1,   n) nn         = Jmp  (toAddress n nn)
toInstruction (0x2,   n) nn         = Call (toAddress n nn)
toInstruction (0x3,   x) nn         = SkipEq    (fromIntegral x) (NN $ fromNib nn)
toInstruction (0x4,   x) nn         = SkipNotEq (fromIntegral x) (NN $ fromNib nn)
toInstruction (0x5,   x) (y,   0x0) = SkipEq    (fromIntegral x) (VI $ fromIntegral y)
toInstruction (0x6,   x) nn         = Set (fromIntegral x) (NN $ fromNib nn)
toInstruction (0x7,   x) nn         = Add (fromIntegral x) (NN $ fromNib nn)
toInstruction (0x8,   x) (y,   0x0) = Set (fromIntegral x) (VI $ fromIntegral y)
toInstruction (0x8,   x) (y,   0x1) = Or  (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0x2) = And (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0x3) = Xor (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0x4) = Add (fromIntegral x) (VI $ fromIntegral y)
toInstruction (0x8,   x) (y,   0x5) = Sub (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0x6) = ShiftRight (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0x7) = SubN (fromIntegral x) (fromIntegral y)
toInstruction (0x8,   x) (y,   0xE) = ShiftLeft  (fromIntegral x) (fromIntegral y)
toInstruction (0x9,   x) (y,   0x0) = SkipNotEq  (fromIntegral x) (VI $ fromIntegral y)
toInstruction (0xA,   n) nn         = Index (toAddress n nn)
toInstruction (0xC,   x) nn         = Rand  (fromIntegral x) (fromNib nn)
toInstruction (0xD,   x) (y,     n) = Draw  (fromIntegral x) (fromIntegral y) (fromIntegral n)
toInstruction (0xE,   x) (0x9, 0xE) = SkipKey     (fromIntegral x)
toInstruction (0xE,   x) (0xA, 0x1) = SkipNotKey  (fromIntegral x)
toInstruction (0xF,   x) (0x0, 0x7) = GetDelay    (fromIntegral x)
toInstruction (0xF,   x) (0x1, 0x5) = SetDelay    (fromIntegral x)
toInstruction (0xF,   x) (0x1, 0x8) = Sound       (fromIntegral x)
toInstruction (0xF,   x) (0x1, 0xE) = AddIndex    (fromIntegral x)
toInstruction (0xF,   x) (0x2, 0x9) = Font        (fromIntegral x)
toInstruction (0xF,   x) (0x0, 0xA) = GetKey      (fromIntegral x)
toInstruction (0xF,   x) (0x3, 0x3) = BCDConv     (fromIntegral x)
toInstruction (0xF,   x) (0x5, 0x5) = WriteMemory (fromIntegral x)
toInstruction (0xF,   x) (0x6, 0x5) = ReadMemory  (fromIntegral x)
toInstruction (n0,   n1) (n2,   n3) = error $ "CPU: unsupported instruction: " <> byte
    where byte = foldMap (toText . flip showHex "") [n0, n1, n2, n3]

toAddress :: Word8 -> Nibbles -> Int
toAddress n nn = (fromIntegral n `shiftL` 8) .|. fromIntegral (fromNib nn)

toNib :: Word8 -> Nibbles
toNib w = over both (.&. 0xF) (w `shiftR` 4, w)

fromNib :: Nibbles -> Word8
fromNib (h, l) = (fromIntegral h `shiftL` 4) .|. fromIntegral l
