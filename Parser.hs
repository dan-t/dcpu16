
module Parser where

import Control.Applicative ((<*>), (<$>), (*>), (<*), (<|>), pure)
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as PC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word (Word16)
import Data.Char (isAlpha)
import qualified CommonTypes as CT

parse :: Text -> P.Result Program
parse text = P.feed (P.parse program text) T.empty

program :: P.Parser Program
program = PC.manyTill line P.endOfInput

line :: P.Parser Line
line = PC.manyTill (skipWS *> statement) (P.endOfLine <|> P.endOfInput)

statement :: P.Parser Statement
statement = instruction         <|>
            nonBasicInstruction <|>
            label               <|>
            comment

instruction :: P.Parser Statement
instruction = Instruction <$> opcode <*>
                 (skipWS *> value) <*>
                 (P.char ',' *> skipWS *> value)

nonBasicInstruction :: P.Parser Statement
nonBasicInstruction = NonBasicInstruction <$>
                         nonBasicOpcode <*>
                         (skipWS *> value)

label :: P.Parser Statement
label = Label <$> (P.char ':' *> P.takeWhile1 isAlpha)

comment :: P.Parser Statement
comment = Comment <$> (P.char ';' *> skipWS *> P.takeTill P.isEndOfLine)

nonBasicOpcode :: P.Parser CT.NonBasicOpcode
nonBasicOpcode = str "JSR" *> pure CT.JSR

opcode :: P.Parser CT.Opcode
opcode = str "SET" *> pure CT.SET <|>
         str "ADD" *> pure CT.ADD <|>
         str "SUB" *> pure CT.SUB <|>
         str "MUL" *> pure CT.MUL <|>
         str "DIV" *> pure CT.DIV <|>
         str "MOD" *> pure CT.MOD <|>
         str "SHL" *> pure CT.SHL <|>
         str "SHR" *> pure CT.SHR <|>
         str "AND" *> pure CT.AND <|>
         str "BOR" *> pure CT.BOR <|>
         str "XOR" *> pure CT.XOR <|>
         str "IFE" *> pure CT.IFE <|>
         str "IFN" *> pure CT.IFN <|>
         str "IFG" *> pure CT.IFG <|>
         str "IFB" *> pure CT.IFB

value :: P.Parser Value
value = ramAt <|> register <|> sp <|> pc <|> o <|> pop <|>
        peek <|> push <|> (Literal <$> literal) <|> labelValue

labelValue :: P.Parser Value
labelValue = LabelValue <$> P.takeWhile1 isAlpha

ramAt :: P.Parser Value
ramAt = RAM <$> (P.char '[' *> skipWS *> value <* skipWS <* P.char ']')

literal :: P.Parser Word16
literal = (str "0x" *> P.hexadecimal) <|> P.decimal

register :: P.Parser Value
register = Register <$>
              (P.char 'A' *> pure CT.A <|>
               P.char 'B' *> pure CT.B <|>
               P.char 'C' *> pure CT.C <|>
               P.char 'X' *> pure CT.X <|>
               P.char 'Y' *> pure CT.Y <|>
               P.char 'Z' *> pure CT.Z <|>
               P.char 'I' *> pure CT.I <|>
               P.char 'J' *> pure CT.J)

sp = str "SP" *> pure SP
pc = str "PC" *> pure PC
o = P.char 'O' *> pure O
pop = str "POP" *> pure POP
peek = str "PEEK" *> pure PEEK
push = str "PUSH" *> pure PUSH

str :: String -> P.Parser Text
str s = P.string $ T.pack s

skipWS = P.skipWhile P.isHorizontalSpace

ps parser str = P.feed (P.parse parser (T.pack str)) T.empty

type Program = [Line]

type Line = [Statement]

data Statement = Instruction CT.Opcode Value Value
                 | NonBasicInstruction CT.NonBasicOpcode Value
                 | Label Text
                 | Comment Text
                 deriving (Show)

data Value = RAM Value | Register CT.RegName | SP | PC | O |
             POP | PEEK | PUSH | Literal Word16 | LabelValue Text
             deriving (Show)
