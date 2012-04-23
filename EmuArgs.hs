{-# LANGUAGE DeriveDataTypeable #-}

module EmuArgs where
import System.Console.CmdArgs

data Emu16 = Emu16 {
   verbose :: Bool,
   binary  :: FilePath
   } deriving (Data, Typeable, Show, Eq)


emu16 = Emu16 {
   verbose = def &= help "print each executed instruction" &= name "V",
   binary  = def &= args &= typ "BINARY_FILE"
   }
   &= summary summaryInfo
   &= help "An emulator for the dcpu16 (http://0x10c.com/doc/dcpu-16.txt)."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo ]

versionInfo = "emu16 version 0.1"
summaryInfo = ""


emu16Args :: IO Emu16
emu16Args = cmdArgs emu16
