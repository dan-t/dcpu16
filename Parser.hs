{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ((<*>), (<$>), (*>), (<*), (<|>), pure)
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as PC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word (Word16)
import Data.Char (isAlpha)
import qualified CommonTypes as CT

parse :: Text -> Program
parse text =
   case P.feed (P.parse program text) T.empty of
        P.Fail _ cs e -> error $ "Parsing failed because of: " ++ show e ++ ", at: " ++ show cs
        P.Partial _   -> error "Parsing only partially finished!"
        P.Done _ r    -> r

program :: P.Parser Program
program = PC.manyTill line P.endOfInput

line :: P.Parser Line
line = PC.manyTill (skipWS *> statement) (P.endOfLine <|> P.endOfInput)

statement :: P.Parser Statement
statement = instruction
            <|> nonBasicInstruction
            <|> label
            <|> comment

instruction :: P.Parser Statement
instruction = Instruction
                 <$> opcode
                 <*> (skipWS *> value)
                 <*> (P.char ',' *> skipWS *> value)

nonBasicInstruction :: P.Parser Statement
nonBasicInstruction = NonBasicInstruction
                         <$> nonBasicOpcode
                         <*> (skipWS *> value)

label :: P.Parser Statement
label = Label <$> (P.char ':' *> P.takeWhile1 isAlpha)

comment :: P.Parser Statement
comment = Comment <$> (P.char ';' *> skipWS *> P.takeTill P.isEndOfLine)

nonBasicOpcode :: P.Parser CT.NonBasicOpcode
nonBasicOpcode = P.string "JSR" *> pure CT.JSR

opcode :: P.Parser CT.Opcode
opcode = P.string "SET" *> pure CT.SET
         <|> P.string "ADD" *> pure CT.ADD
         <|> P.string "SUB" *> pure CT.SUB
         <|> P.string "MUL" *> pure CT.MUL
         <|> P.string "DIV" *> pure CT.DIV
         <|> P.string "MOD" *> pure CT.MOD
         <|> P.string "SHL" *> pure CT.SHL
         <|> P.string "SHR" *> pure CT.SHR
         <|> P.string "AND" *> pure CT.AND
         <|> P.string "BOR" *> pure CT.BOR
         <|> P.string "XOR" *> pure CT.XOR
         <|> P.string "IFE" *> pure CT.IFE
         <|> P.string "IFN" *> pure CT.IFN
         <|> P.string "IFG" *> pure CT.IFG
         <|> P.string "IFB" *> pure CT.IFB

value :: P.Parser Value
value = ramValue
        <|> (Register <$> register)
        <|> sp
        <|> pc
        <|> o
        <|> pop
        <|> peek
        <|> push
        <|> (Literal <$> literal)
        <|> (LabelValue <$> labelText)

labelText :: P.Parser Text
labelText = P.takeWhile1 isAlpha

ramValue :: P.Parser Value
ramValue = RamValue <$> (P.char '[' *> skipWS *> ramAddress <* skipWS <* P.char ']')

ramAddress :: P.Parser RamAddress
ramAddress = ((\(l, rn) -> AtLiteralPlusReg l rn) <$> literalPlusReg)
             <|> (AtRegister <$> register)
             <|> (AtLiteral <$> literal)
             <|> (AtLabel <$> labelText)

literalPlusReg :: P.Parser (Word16, CT.RegName)
literalPlusReg = ((,) <$> literal <*> (plus *> register))
                 <|> (swap <$> register <*> (plus *> literal))
   where
      swap = \rn l -> (l, rn)
      plus = skipWS *> P.char '+' *> skipWS


literal :: P.Parser Word16
literal = (P.string "0x" *> P.hexadecimal) <|> P.decimal

register :: P.Parser CT.RegName
register = P.char 'A' *> pure CT.A
           <|> P.char 'B' *> pure CT.B
           <|> P.char 'C' *> pure CT.C
           <|> P.char 'X' *> pure CT.X
           <|> P.char 'Y' *> pure CT.Y
           <|> P.char 'Z' *> pure CT.Z
           <|> P.char 'I' *> pure CT.I
           <|> P.char 'J' *> pure CT.J

sp = P.string "SP" *> pure SP
pc = P.string "PC" *> pure PC
o = P.char 'O' *> pure O
pop = P.string "POP" *> pure POP
peek = P.string "PEEK" *> pure PEEK
push = P.string "PUSH" *> pure PUSH

skipWS = P.skipWhile P.isHorizontalSpace

ps parser str = P.feed (P.parse parser $ T.pack str) T.empty

type Program = [Line]

type Line = [Statement]

data Statement = Instruction CT.Opcode Value Value
                 | NonBasicInstruction CT.NonBasicOpcode Value
                 | Label Text
                 | Comment Text
                 deriving (Show)

data Value = RamValue RamAddress | Register CT.RegName | SP | PC | O
             | POP | PEEK | PUSH | Literal Word16 | LabelValue Text
             deriving (Show)

data RamAddress = AtLiteral Word16 | AtRegister CT.RegName | AtLabel Text
                  | AtLiteralPlusReg Word16 CT.RegName
                  deriving (Show)
