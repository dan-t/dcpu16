
module Parser where

import Control.Applicative
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as PC
import Data.Word
import Data.Text as T
import Data.Char

parse :: T.Text -> P.Result Program
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

nonBasicOpcode :: P.Parser NonBasicOpcode
nonBasicOpcode = str "JSR" *> pure JSR

opcode :: P.Parser Opcode
opcode = str "SET" *> pure SET <|>
         str "ADD" *> pure ADD <|>
         str "SUB" *> pure SUB <|>
         str "MUL" *> pure MUL <|>
         str "DIV" *> pure DIV <|>
         str "MOD" *> pure MOD <|>
         str "SHL" *> pure SHL <|>
         str "SHR" *> pure SHR <|>
         str "AND" *> pure AND <|>
         str "BOR" *> pure BOR <|>
         str "XOR" *> pure XOR <|>
         str "IFE" *> pure IFE <|>
         str "IFN" *> pure IFN <|>
         str "IFG" *> pure IFG <|>
         str "IFB" *> pure IFB

value :: P.Parser Value
value = ramAt <|> register <|> sp <|> pc <|> o <|> pop <|>
        peek <|> push <|> (Literal <$> literal) <|> labelValue

labelValue :: P.Parser Value
labelValue = LabelValue <$> P.takeWhile1 isAlpha

ramAt :: P.Parser Value
ramAt = RAM <$> (P.char '[' *> skipWS *> literal <* skipWS <* P.char ']')

literal :: P.Parser Word16
literal = (str "0x" *> P.hexadecimal) <|> P.decimal

register :: P.Parser Value
register = Register <$>
              (P.char 'A' *> pure A <|>
               P.char 'B' *> pure B <|>
               P.char 'C' *> pure C <|>
               P.char 'X' *> pure X <|>
               P.char 'Y' *> pure Y <|>
               P.char 'Z' *> pure Z <|>
               P.char 'I' *> pure I <|>
               P.char 'J' *> pure J)

sp = str "SP" *> pure SP
pc = str "PC" *> pure PC
o = P.char 'O' *> pure O
pop = str "POP" *> pure POP
peek = str "PEEK" *> pure PEEK
push = str "PUSH" *> pure PUSH

str :: String -> P.Parser Text
str s = P.string $ pack s

skipWS = P.skipWhile P.isHorizontalSpace

ps parser str = P.feed (P.parse parser (T.pack str)) T.empty

type Program = [Line]

type Line = [Statement]

data Statement = Instruction Opcode Value Value
                 | NonBasicInstruction NonBasicOpcode Value
                 | Label Text
                 | Comment Text
                 deriving (Show)

data Opcode = SET | ADD | SUB | MUL | DIV | MOD | SHL |
              SHR | AND | BOR | XOR | IFE | IFN | IFG | IFB
              deriving (Show)

data NonBasicOpcode = JSR deriving (Show)

data Value = RAM Word16 | Register RegName | SP | PC | O |
             POP | PEEK | PUSH | Literal Word16 | LabelValue Text
             deriving (Show)

data RegName = A | B | C | X | Y | Z | I | J deriving (Show)
