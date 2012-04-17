
module CommonTypes where

import Data.Bits ((.&.))

data Opcode = SET | ADD | SUB | MUL | DIV | MOD | SHL |
              SHR | AND | BOR | XOR | IFE | IFN | IFG | IFB
              deriving (Show, Eq, Ord)

opcode word
   | op ==  1  = SET
   | op ==  2  = ADD
   | op ==  3  = SUB
   | op ==  4  = MUL
   | op ==  5  = DIV
   | op ==  6  = MOD
   | op ==  7  = SHL
   | op ==  8  = SHR
   | op ==  9  = AND
   | op == 10  = BOR
   | op == 11  = XOR
   | op == 12  = IFE
   | op == 13  = IFN
   | op == 14  = IFG
   | op == 15  = IFB
   | otherwise = error $ "Invalid basic opcode: " ++ show op
   where
      op = word .&. 0xf


data NonBasicOpcode = JSR deriving (Show, Eq)

nonBasicOpcode word
   | op == 1   = JSR
   | otherwise = error $ "Invalid non basic opcode: " ++ show op
   where
      op = word .&. 0xf


data RegName = A | B | C | X | Y | Z | I | J deriving (Show, Eq, Enum)
