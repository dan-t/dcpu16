{-# LANGUAGE NoMonomorphismRestriction #-}

module Binary where

import Data.Bits
import Data.Char
import Data.Word
import Data.List
import Numeric

regA = 0x00 :: Word32
regB = 0x01 :: Word32
regC = 0x02 :: Word32
regX = 0x03 :: Word32
regY = 0x04 :: Word32
regZ = 0x05 :: Word32
regI = 0x06 :: Word32
regJ = 0x07 :: Word32

ram :: Word32 -> Word32
ram addr = (addr .<<. 16) .|. 0x1e

pop  = 0x18 :: Word32
peek = 0x19 :: Word32
push = 0x1a :: Word32
sp   = 0x1b :: Word32
pc   = 0x1c :: Word32
o    = 0x1d :: Word32

lit :: Word32 -> Word32
lit i
   | i > 0x1f  = (i .<<. 16) .|. 0x1f
   | otherwise = i + 0x20

jsr :: Word32 -> [Word16]
jsr a
   | nextWordA = [firstWord, secondWord]
   | otherwise = [firstWord]
   where
      firstWord  = toW16 $ (0x01 .<<. 4) .|. ((a .&. 0x3f) .<<. 10)
      secondWord = toW16 $ a .>>. 16
      nextWordA  = a .&. 0xffff0000 /= 0


set = instruc 0x1
add = instruc 0x2
sub = instruc 0x3
mul = instruc 0x4
div = instruc 0x5
mod = instruc 0x6
shl = instruc 0x7
shr = instruc 0x8
and = instruc 0x9
bor = instruc 0xa
xor = instruc 0xb
ife = instruc 0xc
ifn = instruc 0xd
ifg = instruc 0xe
ifb = instruc 0xf

instruc :: Word16 -> Word32 -> Word32 -> [Word16]
instruc opcode a b
   | nextWordA && nextWordB = [firstWord, toW16 $ a .>>. 16, toW16 $ b .>>. 16]
   | nextWordA              = [firstWord, toW16 $ a .>>. 16]
   | nextWordB              = [firstWord, toW16 $ b .>>. 16]
   | otherwise              = [firstWord]
   where
      firstWord = opcode .|. toW16 ((a .&. 0x3f) .<<. 4) .|. toW16 ((b .&. 0x3f) .<<. 10)
      nextWordA = a .&. 0xffff0000 /= 0
      nextWordB = b .&. 0xffff0000 /= 0

hex = intercalate " " . map (\w -> showHex w "")
bin = intercalate " " . map (\w -> showIntAtBase 2 intToDigit w "")

(.>>.) = shiftR
(.<<.) = shiftL

toW16 :: Integral a => a -> Word16
toW16 = fromIntegral
