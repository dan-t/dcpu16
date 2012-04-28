{-# LANGUAGE NoMonomorphismRestriction #-}

module BinaryCode where

import Data.Bits hiding (xor)
import Data.Char
import Data.Word
import qualified Data.DList as DL
import Control.Monad.Writer
import Numeric
import qualified Data.List as L
import qualified CommonTypes as CT
import Prelude hiding (div, mod, and)

type BinaryCode = Writer (DL.DList Word16)
binaryCode = DL.toList . execWriter

setM = basicInstrucM set
addM = basicInstrucM add
subM = basicInstrucM sub
mulM = basicInstrucM mul
divM = basicInstrucM div
modM = basicInstrucM mod
shlM = basicInstrucM shl
shrM = basicInstrucM shr
andM = basicInstrucM and
borM = basicInstrucM bor
xorM = basicInstrucM xor
ifeM = basicInstrucM ife
ifnM = basicInstrucM ifn
ifgM = basicInstrucM ifg
ifbM = basicInstrucM ifb

basicInstrucM :: Word16 -> Word32 -> Word32 -> BinaryCode ()
basicInstrucM opcode a b
   | nextWordA && nextWordB = tell $ DL.fromList [firstWord, toW16 $ a .>>. 16, toW16 $ b .>>. 16]
   | nextWordA              = tell $ DL.fromList [firstWord, toW16 $ a .>>. 16]
   | nextWordB              = tell $ DL.fromList [firstWord, toW16 $ b .>>. 16]
   | otherwise              = tell $ DL.fromList [firstWord]
   where
      firstWord = opcode .|. toW16 ((a .&. 0x3f) .<<. 4) .|. toW16 ((b .&. 0x3f) .<<. 10)
      nextWordA = a .&. 0xffff0000 /= 0
      nextWordB = b .&. 0xffff0000 /= 0

jsrM = nonBasicInstrucM jsr

nonBasicInstrucM :: Word16 -> Word32 -> BinaryCode ()
nonBasicInstrucM opcode a
   | nextWordA = tell $ DL.fromList [firstWord, secondWord]
   | otherwise = tell $ DL.fromList [firstWord]
   where
      firstWord  = (opcode .<<. 4) .|. toW16 ((a .&. 0x3f) .<<. 10)
      secondWord = toW16 $ a .>>. 16
      nextWordA  = a .&. 0xffff0000 /= 0

opcode opcode =
   case opcode of
        CT.SET -> set
        CT.ADD -> add
        CT.SUB -> sub
        CT.MUL -> mul
        CT.DIV -> div
        CT.MOD -> mod
        CT.SHL -> shl
        CT.SHR -> shr
        CT.AND -> and
        CT.BOR -> bor
        CT.XOR -> xor
        CT.IFE -> ife
        CT.IFN -> ifn
        CT.IFG -> ifg
        CT.IFB -> ifb

nonBasicOpcode opcode =
   case opcode of
        CT.JSR -> jsr

reg name =
   case name of
        CT.A -> regA
        CT.B -> regB
        CT.C -> regC
        CT.X -> regX
        CT.Y -> regY
        CT.Z -> regZ
        CT.I -> regI
        CT.J -> regJ

ramAt :: Word32 -> Word32
ramAt addr = (addr .<<. 16) .|. 0x1e

lit :: Word32 -> Word32
lit i
   | i > 0x1f  = (i .<<. 16) .|. 0x1f
   | otherwise = i + 0x20

set = 0x1
add = 0x2
sub = 0x3
mul = 0x4
div = 0x5
mod = 0x6
shl = 0x7
shr = 0x8
and = 0x9
bor = 0xa
xor = 0xb
ife = 0xc
ifn = 0xd
ifg = 0xe
ifb = 0xf

jsr = 0x01

pop  = 0x18
peek = 0x19
push = 0x1a
sp   = 0x1b
pc   = 0x1c
o    = 0x1d

literalAtNextWord = 0x1f
ramAtNextWord     = 0x1e
literalOffset     = 0x20
ramAtRegOffset    = 0x08

regA = 0x00
regB = 0x01
regC = 0x02
regX = 0x03
regY = 0x04
regZ = 0x05
regI = 0x06
regJ = 0x07

hex = unwords . L.map (`showHex` "")
bin = unwords . L.map (\w -> showIntAtBase 2 intToDigit w "")

(.>>.) = shiftR
(.<<.) = shiftL

toW16 :: Integral a => a -> Word16
toW16 = fromIntegral
