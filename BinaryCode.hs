{-# LANGUAGE NoMonomorphismRestriction #-}

module BinaryCode where

import Data.Bits
import Data.Char
import Data.Word
import Data.List
import Control.Monad.Writer
import Numeric

type BinaryCode = Writer [Word16]
binaryCode = execWriter

basicInstrucB :: Word16 -> Word32 -> Word32 -> BinaryCode ()
basicInstrucB opcode a b = tell $ bBasicInstruc opcode a b

setB = basicInstrucB 0x1
addB = basicInstrucB 0x2
subB = basicInstrucB 0x3
mulB = basicInstrucB 0x4
divB = basicInstrucB 0x5
modB = basicInstrucB 0x6
shlB = basicInstrucB 0x7
shrB = basicInstrucB 0x8
andB = basicInstrucB 0x9
borB = basicInstrucB 0xa
xorB = basicInstrucB 0xb
ifeB = basicInstrucB 0xc
ifnB = basicInstrucB 0xd
ifgB = basicInstrucB 0xe
ifbB = basicInstrucB 0xf


nonBasicInstrucB :: Word16 -> Word32 -> BinaryCode ()
nonBasicInstrucB opcode a = tell $ bNonBasicInstruc opcode a

jsrB = nonBasicInstrucB 0x01


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


bBasicInstruc :: Word16 -> Word32 -> Word32 -> [Word16]
bBasicInstruc opcode a b
   | nextWordA && nextWordB = [firstWord, toW16 $ a .>>. 16, toW16 $ b .>>. 16]
   | nextWordA              = [firstWord, toW16 $ a .>>. 16]
   | nextWordB              = [firstWord, toW16 $ b .>>. 16]
   | otherwise              = [firstWord]
   where
      firstWord = opcode .|. toW16 ((a .&. 0x3f) .<<. 4) .|. toW16 ((b .&. 0x3f) .<<. 10)
      nextWordA = a .&. 0xffff0000 /= 0
      nextWordB = b .&. 0xffff0000 /= 0


bset = bBasicInstruc 0x1
badd = bBasicInstruc 0x2
bsub = bBasicInstruc 0x3
bmul = bBasicInstruc 0x4
bdiv = bBasicInstruc 0x5
bmod = bBasicInstruc 0x6
bshl = bBasicInstruc 0x7
bshr = bBasicInstruc 0x8
band = bBasicInstruc 0x9
bbor = bBasicInstruc 0xa
bxor = bBasicInstruc 0xb
bife = bBasicInstruc 0xc
bifn = bBasicInstruc 0xd
bifg = bBasicInstruc 0xe
bifb = bBasicInstruc 0xf


bNonBasicInstruc :: Word16 -> Word32 -> [Word16]
bNonBasicInstruc opcode a
   | nextWordA = [firstWord, secondWord]
   | otherwise = [firstWord]
   where
      firstWord  = (opcode .<<. 4) .|. toW16 ((a .&. 0x3f) .<<. 10)
      secondWord = toW16 $ a .>>. 16
      nextWordA  = a .&. 0xffff0000 /= 0

bjsr = bNonBasicInstruc 0x01


hex = intercalate " " . map (\w -> showHex w "")
bin = intercalate " " . map (\w -> showIntAtBase 2 intToDigit w "")

(.>>.) = shiftR
(.<<.) = shiftL

toW16 :: Integral a => a -> Word16
toW16 = fromIntegral
