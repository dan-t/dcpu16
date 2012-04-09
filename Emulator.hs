{-# LANGUAGE NoMonomorphismRestriction #-}

module Emulator where

import Data.Vector.Unboxed as V hiding ((++))
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Control.Monad.State.Strict as S
import Control.Monad
import System.Environment
import Numeric
import Prelude as P


execStep :: DCPU_Data -> IO DCPU_Data
execStep dcpu = do
   let (instruc, dcpu') = S.runState readInstruction dcpu
   putStrLn $ show instruc
   let dcpu'' = S.execState (execInstruction instruc) dcpu'
   putStrLn $ show dcpu''
   return dcpu''


execInstruction :: Instruction -> DCPU ()
execInstruction (BasicInstruction opcode (valA, locA) valB)
   | opcode == NonBasicOp = error $ "Unexpected non basic opcode!"

   | opcode <= XOR = do
      let (a, b)     = (toW32 valA, toW32 valB)
          (res, ovf) = case opcode of
                            SET -> (b, 0)
                            ADD -> let r = a+b in (r, r .>>. 16)
                            SUB -> let r = a-b in (r, r .>>. 16)
                            MUL -> let r = a*b in (r, r .>>. 16)
                            DIV -> if b == 0
                                      then (0, 0)
                                      else let r = a `P.div` b
                                           in (r, (a .<<. 16) `P.div` b)

                            MOD -> if b == 0 then (0, 0) else (a `mod` b, 0)
                            SHL -> let r = a .<<. (toInt b) in (r, r .>>. 16)
                            SHR -> let r = a .>>. (toInt b) in (r, (a .<<. 16) .>>. (toInt b))
                            AND -> (a .&. b, 0)
                            BOR -> (a .|. b, 0)
                            XOR -> (a `xor` b, 0)

      setValue (toW16 res) locA
      S.modify (\dcpu -> dcpu {ov = toW16 ovf})

   | opcode == IFE = unless (valA == valB)       $ readWord >> return ()
   | opcode == IFN = unless (valA /= valB)       $ readWord >> return ()
   | opcode == IFG = unless (valA >  valB)       $ readWord >> return ()
   | opcode == IFB = unless (valA .&. valB /= 0) $ readWord >> return ()

   | otherwise = error $ "Unexpected case in execInstruction!"

   where
      toInt :: Integral a => a -> Int
      toInt = fromIntegral

      toW32 :: Integral a => a -> Word32
      toW32 = fromIntegral

      toW16 :: Integral a => a -> Word16
      toW16 = fromIntegral


execInstruction (NonBasicInstruction opcode val) = do
   case opcode of
        JSR -> return ()
        InvalidNonBasicOp -> error $ "Executing invalid non basic instruction!"


readInstruction :: DCPU Instruction
readInstruction = do
   word <- readWord
   case opcode word of
        NonBasicOp | valA word /= 0x01 -> error $ "Invalid non basic opcode: " ++ (showHex (valA word) "")
                   | otherwise -> do (val, _) <- readValue $ valB word
                                     return $! NonBasicInstruction JSR val

        basicOp -> do a      <- readValue $ valA word
                      (b, _) <- readValue $ valB word
                      return $! BasicInstruction basicOp a b
   where
      opcode = toEnum . fromIntegral . (.&. 0xf)
      valA   = (.&. 0x3f) . (.>>. 4)
      valB   = (.&. 0x3f) . (.>>. 10)


readValue :: Word16 -> DCPU (Word16, Location)
readValue word
   | word <= 0x07 = do let reg = toEnum $ fromIntegral word
                       val <- readRegister reg
                       return $! (val, Register reg)

   | word <= 0x0f = do let reg = toEnum $ fromIntegral (word - 0x08)
                       addr <- readRegister reg
                       ramVal <- readRam addr
                       return $! (ramVal, RAM addr)

   | word <= 0x17 = do let reg = toEnum $ fromIntegral (word - 0x10)
                       regVal <- readRegister reg
                       nextWord <- readWord
                       let addr = nextWord + regVal
                       ramVal <- readRam addr
                       return $! (ramVal, RAM addr)

   | word == 0x18 = S.state (\dcpu ->
      let sp_ = sp dcpu
          in ((get (ram dcpu) sp_, RAM sp_), dcpu {sp = sp_ + 1}))

   | word == 0x19 = S.gets (\dcpu ->
      let sp_ = sp dcpu in (get (ram dcpu) sp_, RAM sp_))

   | word == 0x1a = S.state (\dcpu ->
      let sp' = (sp dcpu) - 1
          in ((get (ram dcpu) sp', RAM sp'), dcpu {sp = sp'}))

   | word == 0x1b = S.gets (\dcpu -> sp dcpu) >>= (\sp -> return $! (sp, SP))
   | word == 0x1c = S.gets (\dcpu -> pc dcpu) >>= (\pc -> return $! (pc, PC))
   | word == 0x1d = S.gets (\dcpu -> ov dcpu) >>= (\ov -> return $! (ov, O))

   | word == 0x1e = do addr <- readWord
                       val <- readRam addr
                       return $! (val, RAM addr)

   | word == 0x1f = do nextWord <- readWord
                       return $! (nextWord, Literal)

   | otherwise    = return $! (word - 0x20, Literal)


readWord :: DCPU Word16
readWord = do
   dcpu <- S.get
   let word = get (ram dcpu) (pc dcpu)
   S.put dcpu {pc = (pc dcpu) + 1}
   return $! word

readRegister :: RegName -> DCPU Word16
readRegister reg = S.gets (\dcpu -> get (regs dcpu) (fromEnum reg))

readRam :: Word16 -> DCPU Word16
readRam addr = S.gets (\dcpu -> get (ram dcpu) addr)

data Instruction = BasicInstruction Opcode (Value, Location) Value
                   | NonBasicInstruction NonBasicOpcode Value

instance Show Instruction where
   show (BasicInstruction opcode (valA, locA) valB) =
      "(BasicInstruction " ++ (show opcode) ++ " (" ++ (showHex valA "") ++ ", " ++ (show locA) ++ ") " ++ (showHex valB "") ++ ")"
   show (NonBasicInstruction opcode val) =
      "(NonBasicInstruction " ++ (show opcode) ++ " " ++ (showHex val "") ++ ")"

type Value = Word16
data Location = RAM Word16 | Register RegName | SP | PC | O | Literal

instance Show Location where
   show (RAM addr)      = "(RAM " ++ (showHex addr "") ++ ")"
   show (Register name) = "(Register " ++ (show name) ++ ")"
   show SP              = "SP"
   show PC              = "PC"
   show O               = "O"
   show Literal         = "Literal"

data RegName = A | B | C | X | Y | Z | I | J deriving (Show, Eq, Enum)

data Opcode = NonBasicOp | SET | ADD | SUB | MUL | DIV | MOD |
              SHL | SHR | AND | BOR | XOR | IFE | IFN | IFG | IFB
              deriving (Show, Eq, Ord, Enum)

data NonBasicOpcode = InvalidNonBasicOp | JSR deriving (Show, Eq, Enum)

type DCPU = S.State DCPU_Data

data DCPU_Data = DCPU_Data {
   ram  :: V.Vector Word16, -- ram
   regs :: V.Vector Word16, -- registers
   pc   :: Word16,          -- program counter
   sp   :: Word16,          -- stack pointer
   ov   :: Word16           -- overflow
   }

instance Show DCPU_Data where
   show (DCPU_Data ram regs pc sp ov) =
      "DCPU: regs=" ++ (show regs) ++ ", pc=" ++ (showHex pc "") ++
      ", sp=" ++ (showHex sp "") ++ ", ov=" ++ (showHex ov "") ++
      ", ram:" ++ showNonEmptyRam ram

showNonEmptyRam ram = go ram 0
   where
      go ram i
         | i >= V.length ram = ""
         | ram ! i == 0x0    = ""
         | otherwise         = " [" ++ (showHex i "") ++ "]=" ++ (showHex (ram ! i) "")

ramSize    = 0x10000
numRegs    = 8
stackBegin = 0xffff

newDCPU ram_ = DCPU_Data {ram = ram_, regs = V.replicate numRegs 0,
                          pc = 0, sp = stackBegin, ov = 0}

(.>>.) = shiftR
(.<<.) = shiftL

setValue :: Word16 -> Location -> DCPU ()
setValue value (RAM addr)     = S.modify (\dcpu -> dcpu {ram = set (ram dcpu) addr value})
setValue value (Register reg) = S.modify (\dcpu -> dcpu {regs = set (regs dcpu) (fromEnum reg) value})
setValue value PC             = S.modify (\dcpu -> dcpu {pc = value})
setValue value SP             = S.modify (\dcpu -> dcpu {sp = value})
setValue value O              = S.modify (\dcpu -> dcpu {ov = value})
setValue value Literal        = return ()

set vector index value = vector // [(fromIntegral index, value)]
get vector index = vector ! (fromIntegral index)
