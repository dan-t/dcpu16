{-# LANGUAGE NoMonomorphismRestriction #-}

module Binary where

import Data.Bits
import Data.Char
import Data.Word
import Data.List
import Numeric

bregA = 0x00 :: Word32
bregB = 0x01 :: Word32
bregC = 0x02 :: Word32
bregX = 0x03 :: Word32
bregY = 0x04 :: Word32
bregZ = 0x05 :: Word32
bregI = 0x06 :: Word32
bregJ = 0x07 :: Word32

bram :: Word32 -> Word32
bram addr = (addr .<<. 16) .|. 0x1e

bpop  = 0x18 :: Word32
bpeek = 0x19 :: Word32
bpush = 0x1a :: Word32
bsp   = 0x1b :: Word32
bpc   = 0x1c :: Word32
bo    = 0x1d :: Word32

blit :: Word32 -> Word32
blit i
   | i > 0x1f  = (i .<<. 16) .|. 0x1f
   | otherwise = i + 0x20

bjsr :: Word32 -> [Word16]
bjsr a
   | nextWordA = [firstWord, secondWord]
   | otherwise = [firstWord]
   where
      firstWord  = toW16 $ (0x01 .<<. 4) .|. ((a .&. 0x3f) .<<. 10)
      secondWord = toW16 $ a .>>. 16
      nextWordA  = a .&. 0xffff0000 /= 0


bset = instruc 0x1
badd = instruc 0x2
bsub = instruc 0x3
bmul = instruc 0x4
bdiv = instruc 0x5
bmod = instruc 0x6
bshl = instruc 0x7
bshr = instruc 0x8
band = instruc 0x9
bbor = instruc 0xa
bxor = instruc 0xb
bife = instruc 0xc
bifn = instruc 0xd
bifg = instruc 0xe
bifb = instruc 0xf

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
