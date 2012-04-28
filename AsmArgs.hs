{-# LANGUAGE DeriveDataTypeable #-}

module AsmArgs where
import System.Console.CmdArgs

data Asm16 = Asm16 {
   printParsed :: Bool,
   output      :: FilePath,
   assembly    :: FilePath
   } deriving (Data, Typeable, Show, Eq)


asm16 = Asm16 {
   printParsed = def &= help "Print the parsed data",
   output      = def &= help "Specify the output file for the binary" &= typ "FILE",
   assembly    = def &= args &= typ "ASSEMBLY_FILE"
   }
   &= summary summaryInfo
   &= help "An assembler for the dcpu16 (http://0x10c.com/doc/dcpu-16.txt)."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo ]

versionInfo = "asm16 version 0.1"
summaryInfo = ""


asm16Args :: IO Asm16
asm16Args = cmdArgs asm16
