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

setM = basicInstrucM 0x1
addM = basicInstrucM 0x2
subM = basicInstrucM 0x3
mulM = basicInstrucM 0x4
divM = basicInstrucM 0x5
modM = basicInstrucM 0x6
shlM = basicInstrucM 0x7
shrM = basicInstrucM 0x8
andM = basicInstrucM 0x9
borM = basicInstrucM 0xa
xorM = basicInstrucM 0xb
ifeM = basicInstrucM 0xc
ifnM = basicInstrucM 0xd
ifgM = basicInstrucM 0xe
ifbM = basicInstrucM 0xf

jsrM = nonBasicInstrucM 0x01

regA = 0x00 :: Word32
regB = 0x01 :: Word32
regC = 0x02 :: Word32
regX = 0x03 :: Word32
regY = 0x04 :: Word32
regZ = 0x05 :: Word32
regI = 0x06 :: Word32
regJ = 0x07 :: Word32

ramAt :: Word32 -> Word32
ramAt addr = (addr .<<. 16) .|. 0x1e

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


basicInstrucM :: Word16 -> Word32 -> Word32 -> BinaryCode ()
basicInstrucM opcode a b = tell $ basicInstruc opcode a b

nonBasicInstrucM :: Word16 -> Word32 -> BinaryCode ()
nonBasicInstrucM opcode a = tell $ nonBasicInstruc opcode a

basicInstruc :: Word16 -> Word32 -> Word32 -> [Word16]
basicInstruc opcode a b
   | nextWordA && nextWordB = [firstWord, toW16 $ a .>>. 16, toW16 $ b .>>. 16]
   | nextWordA              = [firstWord, toW16 $ a .>>. 16]
   | nextWordB              = [firstWord, toW16 $ b .>>. 16]
   | otherwise              = [firstWord]
   where
      firstWord = opcode .|. toW16 ((a .&. 0x3f) .<<. 4) .|. toW16 ((b .&. 0x3f) .<<. 10)
      nextWordA = a .&. 0xffff0000 /= 0
      nextWordB = b .&. 0xffff0000 /= 0


set = basicInstruc 0x1
add = basicInstruc 0x2
sub = basicInstruc 0x3
mul = basicInstruc 0x4
div = basicInstruc 0x5
mod = basicInstruc 0x6
shl = basicInstruc 0x7
shr = basicInstruc 0x8
and = basicInstruc 0x9
bor = basicInstruc 0xa
xor = basicInstruc 0xb
ife = basicInstruc 0xc
ifn = basicInstruc 0xd
ifg = basicInstruc 0xe
ifb = basicInstruc 0xf

nonBasicInstruc :: Word16 -> Word32 -> [Word16]
nonBasicInstruc opcode a
   | nextWordA = [firstWord, secondWord]
   | otherwise = [firstWord]
   where
      firstWord  = (opcode .<<. 4) .|. toW16 ((a .&. 0x3f) .<<. 10)
      secondWord = toW16 $ a .>>. 16
      nextWordA  = a .&. 0xffff0000 /= 0

jsr = nonBasicInstruc 0x01


hex = intercalate " " . map (\w -> showHex w "")
bin = intercalate " " . map (\w -> showIntAtBase 2 intToDigit w "")

(.>>.) = shiftR
(.<<.) = shiftL

toW16 :: Integral a => a -> Word16
toW16 = fromIntegral
