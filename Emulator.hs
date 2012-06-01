{-# LANGUAGE TupleSections #-}

module Main where

import Data.Vector.Unboxed as V hiding ((++))
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word
import Data.Bits
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Control.Monad.State.Strict as S
import Control.Monad
import Control.Applicative
import Control.Monad.ST
import System.Environment
import Numeric
import BinaryCode (setM, addM, subM, regA, regB, lit, binaryCode)
import CommonTypes
import EmuArgs


main :: IO ()
main = do
   args <- emu16Args
   when (L.null $ binary args) $
      error "No binary file given!"

   bs <- B.readFile (binary args)
   runDCPU (verbose args) $ dcpuFromByteString bs


runSimpleTest :: IO ()
runSimpleTest =
   runDCPU True . dcpuFromList . binaryCode $ do
      setM regA (lit 0x1)
      addM regA (lit 0x2)
      setM regB (lit 0x2)
      subM regA regB


runDCPU :: Bool -> DCPUData -> IO ()
runDCPU verbose dcpu = do
   dcpu' <- execStep verbose dcpu
   when verbose $ print dcpu'
   runDCPU verbose dcpu'


execStep :: Bool -> DCPUData -> IO DCPUData
execStep verbose dcpu = do
   let (instruc, dcpu') = S.runState readInstruction dcpu
   when verbose $ print instruc
   return $! S.execState (execInstruction instruc) dcpu'


dcpuFromByteString :: B.ByteString -> DCPUData
dcpuFromByteString binary =
   DCPUData {ram = initRam binary, regs = V.replicate numRegs 0,
              pc = 0, sp = stackBegin, ov = 0}
   where
      initRam = V.unfoldrN ramSize (\bs ->
         case B.length bs of
              l | l >= 2    -> do (loByte, bs' ) <- B.uncons bs
                                  (hiByte, bs'') <- B.uncons bs'
                                  let loWord = toW16 loByte
                                      hiWord = toW16 hiByte
                                  return (loWord .|. (hiWord .<<. 8), bs'')

                | l == 1    -> do (loByte, bs') <- B.uncons bs
                                  return (toW16 loByte, bs')

                | otherwise -> Just (0, bs))


dcpuFromList :: [Word16] -> DCPUData
dcpuFromList binary =
   DCPUData {ram = initRam binary, regs = V.replicate numRegs 0,
              pc = 0, sp = stackBegin, ov = 0}
   where
      initRam = V.unfoldrN ramSize (\ls ->
         if L.null ls
            then Just (0, ls)
            else Just (L.head ls, L.tail ls))


execInstruction :: Instruction -> DCPU ()
execInstruction (BasicInstruction opcode (valA, locA) valB)
   | opcode <= XOR = do
      let (a, b)     = (toW32 valA, toW32 valB)
          (res, ovf) = case opcode of
                            SET -> (b, 0)
                            ADD -> let r = a+b in (r, r .>>. 16)
                            SUB -> let r = a-b in (r, r .>>. 16)
                            MUL -> let r = a*b in (r, r .>>. 16)
                            DIV -> if b == 0
                                      then (0, 0)
                                      else let r = a `div` b
                                           in (r, (a .<<. 16) `div` b)

                            MOD -> if b == 0 then (0, 0) else (a `mod` b, 0)
                            SHL -> let r = a .<<. toInt b in (r, r .>>. 16)
                            SHR -> let r = a .>>. toInt b in (r, (a .<<. 16) .>>. toInt b)
                            AND -> (a .&. b, 0)
                            BOR -> (a .|. b, 0)
                            XOR -> (a `xor` b, 0)

      setValue (toW16 res) locA
      S.modify (\dcpu -> dcpu {ov = toW16 ovf})

   | opcode == IFE = unless (valA == valB)       $ void readInstruction
   | opcode == IFN = unless (valA /= valB)       $ void readInstruction
   | opcode == IFG = unless (valA >  valB)       $ void readInstruction
   | opcode == IFB = unless (valA .&. valB /= 0) $ void readInstruction

   | otherwise = error "Unexpected case in execInstruction!"


execInstruction (NonBasicInstruction opcode val) =
   case opcode of
        JSR -> S.modify (\dcpu ->
           let sp' = sp dcpu - 1
               in dcpu {ram = set (ram dcpu) sp' (pc dcpu + 1),
                        sp = sp', pc = val})


readInstruction :: DCPU Instruction
readInstruction = do
   word <- readWord
   if word .&. 0xf == 0
      then NonBasicInstruction (nonBasicOpcode $ valA word) . fst <$> readValue (valB word)
      else do a <- readValue $ valA word
              b <- fst <$> readValue (valB word)
              return $! BasicInstruction (opcode word) a b
   where
      valA = (.&. 0x3f) . (.>>. 4)
      valB = (.&. 0x3f) . (.>>. 10)


readValue :: Word16 -> DCPU (Word16, Location)
readValue word
   | word <= 0x07 = do let reg = toEnum $ fromIntegral word
                       (, Register reg) <$> readRegister reg

   | word <= 0x0f = do let reg = toEnum $ fromIntegral (word - 0x08)
                       addr <- readRegister reg
                       (, RAM addr) <$> readRam addr

   | word <= 0x17 = do let reg = toEnum $ fromIntegral (word - 0x10)
                       regVal <- readRegister reg
                       nextWord <- readWord
                       let addr = nextWord + regVal
                       (, RAM addr) <$> readRam addr

   | word == 0x18 = S.state (\dcpu ->
      let sp_ = sp dcpu
          in ((get (ram dcpu) sp_, RAM sp_), dcpu {sp = sp_ + 1}))

   | word == 0x19 = S.gets (\dcpu ->
      let sp_ = sp dcpu in (get (ram dcpu) sp_, RAM sp_))

   | word == 0x1a = S.state (\dcpu ->
      let sp' = sp dcpu - 1
          in ((get (ram dcpu) sp', RAM sp'), dcpu {sp = sp'}))

   | word == 0x1b = (, SP) . sp <$> S.get
   | word == 0x1c = (, PC) . pc <$> S.get
   | word == 0x1d = (,  O) . ov <$> S.get
   | word == 0x1e = readWord >>= \addr -> (, RAM addr) <$> readRam addr
   | word == 0x1f = (, Literal) <$> readWord
   | otherwise    = return (word - 0x20, Literal)


readWord :: DCPU Word16
readWord = do
   dcpu <- S.get
   let word = get (ram dcpu) (pc dcpu)
   S.put dcpu {pc = pc dcpu + 1}
   return $! word

readRegister :: RegName -> DCPU Word16
readRegister reg = S.gets (\dcpu -> get (regs dcpu) (fromEnum reg))

readRam :: Word16 -> DCPU Word16
readRam addr = S.gets (\dcpu -> get (ram dcpu) addr)

data Instruction = BasicInstruction Opcode (Value, Location) Value
                   | NonBasicInstruction NonBasicOpcode Value

instance Show Instruction where
   show (BasicInstruction opcode (valA, locA) valB) =
      "(BasicInstruction " ++ show opcode ++ " (" ++ showHex valA "" ++ ", " ++ show locA ++ ") " ++ showHex valB "" ++ ")"
   show (NonBasicInstruction opcode val) =
      "(NonBasicInstruction " ++ show opcode ++ " " ++ showHex val "" ++ ")"

type Value = Word16
data Location = RAM Word16 | Register RegName | SP | PC | O | Literal

instance Show Location where
   show (RAM addr)      = "(RAM " ++ showHex addr "" ++ ")"
   show (Register name) = "(Register " ++ show name ++ ")"
   show SP              = "SP"
   show PC              = "PC"
   show O               = "O"
   show Literal         = "Literal"

type DCPU = S.State DCPUData

data DCPUData = DCPUData {
   ram  :: ! (V.Vector Word16), -- ram
   regs :: ! (V.Vector Word16), -- registers
   pc   :: ! Word16,            -- program counter
   sp   :: ! Word16,            -- stack pointer
   ov   :: ! Word16             -- overflow
   }

instance Show DCPUData where
   show (DCPUData ram regs pc sp ov) =
      "DCPU: regs=" ++ V.foldl' (\str e -> (if str /= "[" then str ++ "," else str) ++ showHex e "") "[" regs ++ "]" ++
      ", pc=" ++ showHex pc "" ++ ", sp=" ++ showHex sp "" ++ ", ov=" ++ showHex ov ""


showRam f = V.foldl' foldRam ("ram:", 0)
   where
      foldRam (str, addr) cell =
         if f cell
            then (str ++ putCell addr cell, addr + 1)
            else (str, addr + 1)

      putCell addr cell = " [" ++ showHex addr "]=" ++ showHex cell ""


ramSize    = 0x10000
videoRam   = (0x8000, 0x8400)
numRegs    = 8
stackBegin = 0xffff

(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL

setValue :: Word16 -> Location -> DCPU ()
setValue value (RAM addr)     = S.modify (\dcpu -> dcpu {ram = set (ram dcpu) addr value})
setValue value (Register reg) = S.modify (\dcpu -> dcpu {regs = set (regs dcpu) (fromEnum reg) value})
setValue value PC             = S.modify (\dcpu -> dcpu {pc = value})
setValue value SP             = S.modify (\dcpu -> dcpu {sp = value})
setValue value O              = S.modify (\dcpu -> dcpu {ov = value})
setValue value Literal        = return ()


set :: Integral a => V.Vector Word16 -> a -> Word16 -> V.Vector Word16
set vector index value = runST (setM vector (fromIntegral index) value)
   where
      setM vec i val = do
         mVec <- V.unsafeThaw vec
         VM.write mVec i val
         V.unsafeFreeze mVec


get :: Integral a => V.Vector Word16 -> a -> Word16
get vector index = vector ! fromIntegral index

toInt :: Integral a => a -> Int
toInt = fromIntegral

toW32 :: Integral a => a -> Word32
toW32 = fromIntegral

toW16 :: Integral a => a -> Word16
toW16 = fromIntegral
