
module Main where

import Data.Hashable
import Data.Word (Word16, Word8)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Control.Monad.State as S
import qualified CommonTypes as CT
import qualified BinaryCode as B
import qualified Data.ByteString as BS
import qualified Parser as P
import Debug.Trace
import AsmArgs

main :: IO ()
main = do
   args <- asm16Args
   when (L.null $ assembly args) $
      error "No assembly file given!"

   when (L.null $ output args) $
      error "No output file specified!"

   text <- TIO.readFile $ assembly args
   let prog = P.parse text
   when (print_parsed args) $
      print prog

   let bytes = assemble prog
   BS.writeFile (output args) bytes


assemble :: P.Program -> BS.ByteString
assemble prog =
   let asmData    = S.execState (asmProgram prog) emptyAsmData
       words      = map right (binary asmData)
       (bytes, _) = BS.unfoldrN (numBytes asmData) (\(hiByte, words) ->
          if L.null words
             then Nothing
             else let (word:rest)      = words
                      byte | hiByte    = (toW8 $ word `shiftR` 8, (False, rest))
                           | otherwise = (toW8 $ word .&. 0xff  , (True , word:rest))
                      in Just byte)
          (False, words)

       in bytes
   where
      right (Right w) = w
      numBytes        = fromIntegral . (*2) . numWords

      toW8 :: Integral a => a -> Word8
      toW8 = fromIntegral


asmProgram :: P.Program -> Assembler ()
asmProgram prog = mapM_ asmLine prog >> resolveLabels >> reverseBinary

asmLine :: P.Line -> Assembler ()
asmLine line = mapM_ asmStatement line

asmStatement :: P.Statement -> Assembler ()
asmStatement (P.Instruction opcode valueA valueB) = do
   addWord $ B.opcode opcode
   numWs <- S.gets numWords
   addValue valueA (borLastWord . (`shiftL` 4))
   numWs' <- S.gets numWords
   if numWs' > numWs
      then addValue valueB (borSndLastWord . (`shiftL` 10))
      else addValue valueB (borLastWord . (`shiftL` 10))

asmStatement (P.NonBasicInstruction opcode value) = do
   addWord $ B.nonBasicOpcode opcode `shiftL` 4
   addValue value (borLastWord . (`shiftL` 10))

asmStatement s@(P.Label text) = S.modify (\s ->
   s {labelMap = HM.insert text (numWords s) (labelMap s)})

asmStatement (P.Comment _) = return ()

resolveLabels :: Assembler ()
resolveLabels = S.modify (\s ->
   let lMap = labelMap s
       bin' = L.map (either (\l -> maybe (error $ "Couldn't resolve label '" ++ show l ++ "'!")
                                         Right
                                         (HM.lookup l lMap))
                            Right)
                    (binary s)

       in s {binary = bin'})


reverseBinary :: Assembler ()
reverseBinary = S.modify (\s -> s {binary = L.reverse (binary s)})

addValue :: P.Value -> (Word16 -> Assembler ()) -> Assembler ()
addValue v@(P.RAM value) handleWord =
   case value of
        P.RAM _         -> invalidAsmValue
        P.Register name -> handleWord $ B.reg name + B.ramAtRegOffset
        P.Literal word  -> handleWord B.ramAtNextWord >> addWord word
        otherwise       -> handleWord B.ramAtNextWord >> addValue value addWord
   where
      invalidAsmValue = error $ "Invalid assembler value: " ++ show v

addValue (P.Register name)   handleWord = handleWord $ B.reg name
addValue (P.SP)              handleWord = handleWord B.sp
addValue (P.PC)              handleWord = handleWord B.pc
addValue (P.O)               handleWord = handleWord B.o
addValue (P.POP)             handleWord = handleWord B.pop
addValue (P.PEEK)            handleWord = handleWord B.peek
addValue (P.PUSH)            handleWord = handleWord B.push
addValue (P.LabelValue text) handleWord = handleWord B.literalAtNextWord >> addLabel text

addValue (P.Literal word) handleWord
   | word > B.literalAtNextWord = handleWord B.literalAtNextWord >> addWord word
   | otherwise                  = handleWord $ word + B.literalOffset


borLastWord :: Word16 -> Assembler ()
borLastWord word = S.modify (\s ->
   let ((Right h):t) = binary s in s {binary = Right (h .|. word) : t})

borSndLastWord :: Word16 -> Assembler ()
borSndLastWord word = S.modify (\s ->
   let (h:(Right sl):t) = binary s in s {binary = h : Right (sl .|. word) : t})

addWord :: Word16 -> Assembler ()
addWord word = S.modify (\s ->
   s {binary = (Right word) : (binary s), numWords = 1 + numWords s})

addLabel :: T.Text -> Assembler ()
addLabel label = S.modify (\s ->
   s {binary = (Left label) : (binary s), numWords = 1 + numWords s})

type Assembler = S.State AsmData

emptyAsmData = AsmData {binary = [], numWords = 0, labelMap = HM.empty}

data AsmData = AsmData {
   binary   :: [AsmWord],
   numWords :: Word16,
   labelMap :: LabelMap
   } deriving (Show)

dumpAsmData :: Assembler ()
dumpAsmData = do
   asmD <- S.get
   trace (show asmD) return ()

type LabelMap = HM.HashMap T.Text Word16
type AsmWord = Either T.Text Word16
