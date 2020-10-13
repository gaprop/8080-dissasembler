--------------------------
-- Lots of dry code in here
---------------------------

import Data.Char
import Data.Bits
import Numeric (showHex)
import System.Environment
import GHC.Int
import qualified Data.ByteString.Lazy as B

main = do
  args    <- getArgs
  content <- B.readFile (args !! 0)
  putStr . disassemble $ content

pipe f (acc, codes) =
  if codes == B.empty then (acc, B.empty)
  else f (acc, codes)

disassemble :: B.ByteString -> String
disassemble codes = disassemble' ("", codes)
  where 
        disassemble' :: (String, B.ByteString) -> String
        disassemble' (acc, codes) =
          if codes == B.empty then acc
          else
            disassemble'
            $ pipe parseNOP
            $ pipe parseRLC
            $ pipe parseRRC
            $ pipe parseRAL
            $ pipe parseRAR
            $ pipe parseDAA
            $ pipe parseCMA
            $ pipe parseSTC
            $ pipe parseCMC
            $ pipe parseHLT
            $ pipe parseRNZ
            $ pipe parseRZ
            $ pipe parseRET
            $ pipe parseRNC
            $ pipe parseRC
            $ pipe parseRPO
            $ pipe parseXTHL
            $ pipe parseRPE
            $ pipe parsePCHL
            $ pipe parseXCHG
            $ pipe parseRP
            $ pipe parseDI
            $ pipe parseRM
            $ pipe parseSPHL
            $ pipe parseEI
            $ pipe parseMOV
            $ pipe parseADD
            $ pipe parseADC
            $ pipe parseSUB
            $ pipe parseSBB
            $ pipe parseANA
            $ pipe parseXRA
            $ pipe parseORA
            $ pipe parseCMP
            $ pipe parseSTAX
            $ pipe parseINX
            $ pipe parseINR
            $ pipe parseDCR
            $ pipe parseDAD
            $ pipe parseLDAX
            $ pipe parseDCX
            $ pipe parsePOP
            $ pipe parsePUSH
            $ pipe parseRST
            $ pipe parseACI
            $ pipe parseOUT
            $ pipe parseADI
            $ pipe parseSUI
            $ pipe parseIN
            $ pipe parseANI
            $ pipe parseXRI
            $ pipe parseORI
            $ pipe parseCPI
            $ pipe parseSHLD
            $ pipe parseLHLD
            $ pipe parseSTA
            $ pipe parseLDA
            $ pipe parseJNZ
            $ pipe parseJMP
            $ pipe parseCNZ
            $ pipe parseJZ
            $ pipe parseCZ
            $ pipe parseCALL
            $ pipe parseJNC
            $ pipe parseCNC
            $ pipe parseJC
            $ pipe parseCC
            $ pipe parseSBI
            $ pipe parseJPO
            $ pipe parseCPO
            $ pipe parseJPE
            $ pipe parseCPE
            $ pipe parseJP
            $ pipe parseCP
            $ pipe parseJM
            $ pipe parseCM
            $ pipe parseLXI
            $ pipe parseMVI
            $ (acc, codes)

parseSingIns :: String -> String -> String
parseSingIns ins reg = ins ++ " " ++ reg ++ "\n"

-- NOTE: Proly a really dumb way of converting byte string to string
parseNumIns :: Int64 -> B.ByteString -> String -> String -> String
parseNumIns num codes ins reg = ins ++ " " ++ reg ++ ", " ++ (byteStringToStr $ B.take num codes) ++ "\n"

byteStringToStr :: B.ByteString -> String
byteStringToStr = concat . map (toHex . read . show) . B.unpack

toHex :: Int -> String
toHex a
  | a < 16    = "0" ++ showHex a ""
  | otherwise = showHex a ""

-- None operand instructions
----------------------------
parseNOP :: (String, B.ByteString) -> (String, B.ByteString)
parseNOP (parsed, codes) =
  let c = B.head codes
  in case c of
       0x00      -> (parsed ++ "NOP\n", B.tail codes)
       0x08      -> (parsed ++ "NOP\n", B.tail codes)
       0x10      -> (parsed ++ "NOP\n", B.tail codes)
       0x18      -> (parsed ++ "NOP\n", B.tail codes)
       0x20      -> (parsed ++ "NOP\n", B.tail codes)
       0x28      -> (parsed ++ "NOP\n", B.tail codes)
       0x30      -> (parsed ++ "NOP\n", B.tail codes)
       0x38      -> (parsed ++ "NOP\n", B.tail codes)
       0xcb      -> (parsed ++ "NOP\n", B.tail codes)
       0xd9      -> (parsed ++ "NOP\n", B.tail codes)
       0xdd      -> (parsed ++ "NOP\n", B.tail codes)
       0xed      -> (parsed ++ "NOP\n", B.tail codes)
       0xfd      -> (parsed ++ "NOP\n", B.tail codes)
       otherwise -> (parsed, codes)

parseRLC :: (String, B.ByteString) -> (String, B.ByteString)
parseRLC (parsed, codes) = 
  let c = B.head codes
  in case c of 
       0x07       -> (parsed ++ "RLC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRRC :: (String, B.ByteString) -> (String, B.ByteString)
parseRRC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x0f       -> (parsed ++ "RRC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRAL :: (String, B.ByteString) -> (String, B.ByteString)
parseRAL (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x17       -> (parsed ++ "RAL\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRAR :: (String, B.ByteString) -> (String, B.ByteString)
parseRAR (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x1f       -> (parsed ++ "RAR\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseDAA :: (String, B.ByteString) -> (String, B.ByteString)
parseDAA (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x27       -> (parsed ++ "DAA\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseCMA :: (String, B.ByteString) -> (String, B.ByteString)
parseCMA (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x2f       -> (parsed ++ "CMA\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseSTC :: (String, B.ByteString) -> (String, B.ByteString)
parseSTC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x37       -> (parsed ++ "STC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseCMC :: (String, B.ByteString) -> (String, B.ByteString)
parseCMC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x3f       -> (parsed ++ "CMC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseHLT :: (String, B.ByteString) -> (String, B.ByteString)
parseHLT (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x76       -> (parsed ++ "HLT\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRNZ :: (String, B.ByteString) -> (String, B.ByteString)
parseRNZ (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xc0       -> (parsed ++ "RNZ\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRZ :: (String, B.ByteString) -> (String, B.ByteString)
parseRZ (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xc8       -> (parsed ++ "RZ\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRET :: (String, B.ByteString) -> (String, B.ByteString)
parseRET (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xc9       -> (parsed ++ "RET\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRNC :: (String, B.ByteString) -> (String, B.ByteString)
parseRNC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xd0       -> (parsed ++ "RNC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRC :: (String, B.ByteString) -> (String, B.ByteString)
parseRC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xd8       -> (parsed ++ "RC\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRPO :: (String, B.ByteString) -> (String, B.ByteString)
parseRPO (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xe0       -> (parsed ++ "RPO\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseXTHL :: (String, B.ByteString) -> (String, B.ByteString)
parseXTHL (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xe3       -> (parsed ++ "XTHL\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRPE :: (String, B.ByteString) -> (String, B.ByteString)
parseRPE (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xe8       -> (parsed ++ "RPE\n", B.tail codes)
       otherwise  -> (parsed, codes)

parsePCHL :: (String, B.ByteString) -> (String, B.ByteString)
parsePCHL (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xe9       -> (parsed ++ "PCHL\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseXCHG :: (String, B.ByteString) -> (String, B.ByteString)
parseXCHG (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xeb       -> (parsed ++ "XCHG\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRP :: (String, B.ByteString) -> (String, B.ByteString)
parseRP (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xf0       -> (parsed ++ "RP\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseDI :: (String, B.ByteString) -> (String, B.ByteString)
parseDI (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xf3       -> (parsed ++ "DI\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseRM :: (String, B.ByteString) -> (String, B.ByteString)
parseRM (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xf8       -> (parsed ++ "RM\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseSPHL :: (String, B.ByteString) -> (String, B.ByteString)
parseSPHL (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xf9       -> (parsed ++ "SPHL\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseEI :: (String, B.ByteString) -> (String, B.ByteString)
parseEI (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xfb       -> (parsed ++ "EI\n", B.tail codes)
       otherwise  -> (parsed, codes)

parseMOV :: (String, B.ByteString) -> (String, B.ByteString)
parseMOV (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x40      -> (parsed ++ "MOV B, B\n", B.tail codes)
       0x41      -> (parsed ++ "MOV B, C\n", B.tail codes)
       0x42      -> (parsed ++ "MOV B, D\n", B.tail codes)
       0x43      -> (parsed ++ "MOV B, E\n", B.tail codes)
       0x44      -> (parsed ++ "MOV B, H\n", B.tail codes)
       0x45      -> (parsed ++ "MOV B, L\n", B.tail codes)
       0x46      -> (parsed ++ "MOV B, M\n", B.tail codes)
       0x47      -> (parsed ++ "MOV B, A\n", B.tail codes)

       0x48      -> (parsed ++ "MOV C, B\n", B.tail codes)
       0x49      -> (parsed ++ "MOV C, C\n", B.tail codes)
       0x4a      -> (parsed ++ "MOV C, D\n", B.tail codes)
       0x4b      -> (parsed ++ "MOV C, E\n", B.tail codes)
       0x4c      -> (parsed ++ "MOV C, H\n", B.tail codes)
       0x4d      -> (parsed ++ "MOV C, L\n", B.tail codes)
       0x4e      -> (parsed ++ "MOV C, M\n", B.tail codes)
       0x4f      -> (parsed ++ "MOV C, A\n", B.tail codes)

       0x50      -> (parsed ++ "MOV D, B\n", B.tail codes)
       0x51      -> (parsed ++ "MOV D, C\n", B.tail codes)
       0x52      -> (parsed ++ "MOV D, D\n", B.tail codes)
       0x53      -> (parsed ++ "MOV D, E\n", B.tail codes)
       0x54      -> (parsed ++ "MOV D, H\n", B.tail codes)
       0x55      -> (parsed ++ "MOV D, L\n", B.tail codes)
       0x56      -> (parsed ++ "MOV D, M\n", B.tail codes)
       0x57      -> (parsed ++ "MOV D, A\n", B.tail codes)

       0x58      -> (parsed ++ "MOV E, B\n", B.tail codes)
       0x59      -> (parsed ++ "MOV E, C\n", B.tail codes)
       0x5a      -> (parsed ++ "MOV E, D\n", B.tail codes)
       0x5b      -> (parsed ++ "MOV E, E\n", B.tail codes)
       0x5c      -> (parsed ++ "MOV E, H\n", B.tail codes)
       0x5d      -> (parsed ++ "MOV E, L\n", B.tail codes)
       0x5e      -> (parsed ++ "MOV E, M\n", B.tail codes)
       0x5f      -> (parsed ++ "MOV E, A\n", B.tail codes)

       0x60      -> (parsed ++ "MOV H, B\n", B.tail codes)
       0x61      -> (parsed ++ "MOV H, C\n", B.tail codes)
       0x62      -> (parsed ++ "MOV H, D\n", B.tail codes)
       0x63      -> (parsed ++ "MOV H, E\n", B.tail codes)
       0x64      -> (parsed ++ "MOV H, H\n", B.tail codes)
       0x65      -> (parsed ++ "MOV H, L\n", B.tail codes)
       0x66      -> (parsed ++ "MOV H, M\n", B.tail codes)
       0x67      -> (parsed ++ "MOV H, A\n", B.tail codes)

       0x68      -> (parsed ++ "MOV L, B\n", B.tail codes)
       0x69      -> (parsed ++ "MOV L, C\n", B.tail codes)
       0x6a      -> (parsed ++ "MOV L, D\n", B.tail codes)
       0x6b      -> (parsed ++ "MOV L, E\n", B.tail codes)
       0x6c      -> (parsed ++ "MOV L, H\n", B.tail codes)
       0x6d      -> (parsed ++ "MOV L, L\n", B.tail codes)
       0x6e      -> (parsed ++ "MOV L, M\n", B.tail codes)
       0x6f      -> (parsed ++ "MOV L, A\n", B.tail codes)

       0x70      -> (parsed ++ "MOV M, B\n", B.tail codes)
       0x71      -> (parsed ++ "MOV M, C\n", B.tail codes)
       0x72      -> (parsed ++ "MOV M, D\n", B.tail codes)
       0x73      -> (parsed ++ "MOV M, E\n", B.tail codes)
       0x74      -> (parsed ++ "MOV M, H\n", B.tail codes)
       0x75      -> (parsed ++ "MOV M, L\n", B.tail codes)
       0x77      -> (parsed ++ "MOV M, A\n", B.tail codes)

       0x78      -> (parsed ++ "MOV A, B\n", B.tail codes)
       0x79      -> (parsed ++ "MOV A, C\n", B.tail codes)
       0x7a      -> (parsed ++ "MOV A, D\n", B.tail codes)
       0x7b      -> (parsed ++ "MOV A, E\n", B.tail codes)
       0x7c      -> (parsed ++ "MOV A, H\n", B.tail codes)
       0x7d      -> (parsed ++ "MOV A, L\n", B.tail codes)
       0x7e      -> (parsed ++ "MOV A, M\n", B.tail codes)
       0x7f      -> (parsed ++ "MOV A, A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseADD :: (String, B.ByteString) -> (String, B.ByteString)
parseADD (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x80      -> (parsed ++ "ADD B\n", B.tail codes)
       0x81      -> (parsed ++ "ADD C\n", B.tail codes)
       0x82      -> (parsed ++ "ADD D\n", B.tail codes)
       0x83      -> (parsed ++ "ADD E\n", B.tail codes)
       0x84      -> (parsed ++ "ADD H\n", B.tail codes)
       0x85      -> (parsed ++ "ADD L\n", B.tail codes)
       0x86      -> (parsed ++ "ADD M\n", B.tail codes)
       0x87      -> (parsed ++ "ADD A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseADC :: (String, B.ByteString) -> (String, B.ByteString)
parseADC (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x88      -> (parsed ++ "ADC B\n", B.tail codes)
       0x89      -> (parsed ++ "ADC C\n", B.tail codes)
       0x8a      -> (parsed ++ "ADC D\n", B.tail codes)
       0x8b      -> (parsed ++ "ADC E\n", B.tail codes)
       0x8c      -> (parsed ++ "ADC H\n", B.tail codes)
       0x8d      -> (parsed ++ "ADC L\n", B.tail codes)
       0x8e      -> (parsed ++ "ADC M\n", B.tail codes)
       0x8f      -> (parsed ++ "ADC A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseSUB :: (String, B.ByteString) -> (String, B.ByteString)
parseSUB (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x90      -> (parsed ++ "SUB B\n", B.tail codes)
       0x91      -> (parsed ++ "SUB C\n", B.tail codes)
       0x92      -> (parsed ++ "SUB D\n", B.tail codes)
       0x93      -> (parsed ++ "SUB E\n", B.tail codes)
       0x94      -> (parsed ++ "SUB H\n", B.tail codes)
       0x95      -> (parsed ++ "SUB L\n", B.tail codes)
       0x96      -> (parsed ++ "SUB M\n", B.tail codes)
       0x97      -> (parsed ++ "SUB A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseSBB :: (String, B.ByteString) -> (String, B.ByteString)
parseSBB (parsed, codes) = 
  let c = B.head codes
  in case c of
       0x98      -> (parsed ++ "SBB B\n", B.tail codes)
       0x99      -> (parsed ++ "SBB C\n", B.tail codes)
       0x9a      -> (parsed ++ "SBB D\n", B.tail codes)
       0x9b      -> (parsed ++ "SBB E\n", B.tail codes)
       0x9c      -> (parsed ++ "SBB H\n", B.tail codes)
       0x9d      -> (parsed ++ "SBB L\n", B.tail codes)
       0x9e      -> (parsed ++ "SBB M\n", B.tail codes)
       0x9f      -> (parsed ++ "SBB A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseANA :: (String, B.ByteString) -> (String, B.ByteString)
parseANA (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xa0      -> (parsed ++ "ANA B\n", B.tail codes)
       0xa1      -> (parsed ++ "ANA C\n", B.tail codes)
       0xa2      -> (parsed ++ "ANA D\n", B.tail codes)
       0xa3      -> (parsed ++ "ANA E\n", B.tail codes)
       0xa4      -> (parsed ++ "ANA H\n", B.tail codes)
       0xa5      -> (parsed ++ "ANA L\n", B.tail codes)
       0xa6      -> (parsed ++ "ANA M\n", B.tail codes)
       0xa7      -> (parsed ++ "ANA A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseXRA :: (String, B.ByteString) -> (String, B.ByteString)
parseXRA (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xa8      -> (parsed ++ "XRA B\n", B.tail codes)
       0xa9      -> (parsed ++ "XRA C\n", B.tail codes)
       0xaa      -> (parsed ++ "XRA D\n", B.tail codes)
       0xab      -> (parsed ++ "XRA E\n", B.tail codes)
       0xac      -> (parsed ++ "XRA H\n", B.tail codes)
       0xad      -> (parsed ++ "XRA L\n", B.tail codes)
       0xae      -> (parsed ++ "XRA M\n", B.tail codes)
       0xaf      -> (parsed ++ "XRA A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseORA :: (String, B.ByteString) -> (String, B.ByteString)
parseORA (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xb0      -> (parsed ++ "ORA B\n", B.tail codes)
       0xb1      -> (parsed ++ "ORA C\n", B.tail codes)
       0xb2      -> (parsed ++ "ORA D\n", B.tail codes)
       0xb3      -> (parsed ++ "ORA E\n", B.tail codes)
       0xb4      -> (parsed ++ "ORA H\n", B.tail codes)
       0xb5      -> (parsed ++ "ORA L\n", B.tail codes)
       0xb6      -> (parsed ++ "ORA M\n", B.tail codes)
       0xb7      -> (parsed ++ "ORA A\n", B.tail codes)
       otherwise -> (parsed, codes)

parseCMP :: (String, B.ByteString) -> (String, B.ByteString)
parseCMP (parsed, codes) = 
  let c = B.head codes
  in case c of
       0xb8      -> (parsed ++ "CMP B\n", B.tail codes)
       0xb9      -> (parsed ++ "CMP C\n", B.tail codes)
       0xba      -> (parsed ++ "CMP D\n", B.tail codes)
       0xbb      -> (parsed ++ "CMP E\n", B.tail codes)
       0xbc      -> (parsed ++ "CMP H\n", B.tail codes)
       0xbd      -> (parsed ++ "CMP L\n", B.tail codes)
       0xbe      -> (parsed ++ "CMP M\n", B.tail codes)
       0xbf      -> (parsed ++ "CMP A\n", B.tail codes)
       otherwise -> (parsed, codes)
-- One operand instructions
---------------------------
parseSTAX :: (String, B.ByteString) -> (String, B.ByteString)
parseSTAX (parsed, codes) =
  let c = B.head codes
  in case c of
       0x02       -> parse "B"
       0x12       -> parse "D"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "STAX" register, B.tail codes)

parseINX :: (String, B.ByteString) -> (String, B.ByteString)
parseINX (parsed, codes) =
  let c = B.head codes
  in case c of
    0x03       -> parse "B"
    0x13       -> parse "D"
    0x23       -> parse "H"
    0x33       -> parse "SP"
    otherwise  -> (parsed, codes)
    where parse register = (parsed ++ parseSingIns "INX" register, B.tail codes)

parseINR :: (String, B.ByteString) -> (String, B.ByteString)
parseINR (parsed, codes) =
  let c = B.head codes
  in case c of
       0x04       -> parse "B"
       0x0c       -> parse "C"
       0x14       -> parse "D"
       0x1c       -> parse "E"
       0x24       -> parse "H"
       0x2c       -> parse "L"
       0x34       -> parse "M"
       0x3c       -> parse "A"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "INR" register, B.tail codes)

parseDCR :: (String, B.ByteString) -> (String, B.ByteString)
parseDCR (parsed, codes) =
  let c = B.head codes
  in case c of
       0x05 -> parse "B"
       0x0d -> parse "C"
       0x15 -> parse "D"
       0x1d -> parse "E"
       0x25 -> parse "H"
       0x2d -> parse "L"
       0x35 -> parse "M"
       0x3d -> parse "A"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "DCR" register, B.tail codes)

parseDAD :: (String, B.ByteString) -> (String, B.ByteString)
parseDAD (parsed, codes) =
  let c = B.head codes
  in case c of
       0x09 -> parse "B"
       0x19 -> parse "D"
       0x29 -> parse "H"
       0x39 -> parse "SP"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "DAD" register, B.tail codes)

parseLDAX :: (String, B.ByteString) -> (String, B.ByteString)
parseLDAX (parsed, codes) =
  let c = B.head codes
  in case c of
       0x0a -> parse "B"
       0x1a -> parse "D"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "LDAX" register, B.tail codes)

parseDCX :: (String, B.ByteString) -> (String, B.ByteString)
parseDCX (parsed, codes) =
  let c = B.head codes
  in case c of
       0x0b -> parse "B"
       0x1b -> parse "D"
       0x2b -> parse "H"
       0x3b -> parse "SP"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "DCX" register, B.tail codes)

parsePOP :: (String, B.ByteString) -> (String, B.ByteString)
parsePOP (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc1 -> parse "B"
       0xd1 -> parse "D"
       0xe1 -> parse "H"
       0xf1 -> parse "SP"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "POP" register, B.tail codes)

parsePUSH :: (String, B.ByteString) -> (String, B.ByteString)
parsePUSH (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc5 -> parse "B"
       0xd5 -> parse "D"
       0xe5 -> parse "H"
       0xf5 -> parse "SP"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "PUSH" register, B.tail codes)

parseRST :: (String, B.ByteString) -> (String, B.ByteString)
parseRST (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc7 -> parse "0"
       0xcf -> parse "1"
       0xd7 -> parse "2"
       0xdf -> parse "3"
       0xe7 -> parse "4"
       0xef -> parse "5"
       0xf7 -> parse "6"
       0xff -> parse "7"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "RST" register, B.tail codes)

-- On one byte instructions
---------------------------
parseACI :: (String, B.ByteString) -> (String, B.ByteString)
parseACI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xce -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "ACI" register, B.drop 2 codes)

parseOUT :: (String, B.ByteString) -> (String, B.ByteString)
parseOUT (parsed, codes) =
  let c = B.head codes
  in case c of
       0xd3 -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "OUT" register, B.drop 2 codes)

parseADI :: (String, B.ByteString) -> (String, B.ByteString)
parseADI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc6 -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "ADI" register, B.drop 2 codes)

parseSUI :: (String, B.ByteString) -> (String, B.ByteString)
parseSUI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xd6 -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "SUI" register, B.drop 2 codes)

parseIN :: (String, B.ByteString) -> (String, B.ByteString)
parseIN (parsed, codes) =
  let c = B.head codes
  in case c of
       0xdb -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "IN" register, B.drop 2 codes)

parseANI :: (String, B.ByteString) -> (String, B.ByteString)
parseANI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xe6 -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "ANI" register, B.drop 2 codes)

parseXRI :: (String, B.ByteString) -> (String, B.ByteString)
parseXRI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xee -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "XRI" register, B.drop 2 codes)

parseORI :: (String, B.ByteString) -> (String, B.ByteString)
parseORI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xf6 -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "ORI" register, B.drop 2 codes)

parseCPI :: (String, B.ByteString) -> (String, B.ByteString)
parseCPI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xfe -> parse ""
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "CPI" register, B.drop 2 codes)
-- On adress instructions
----------------------
parseSHLD :: (String, B.ByteString) -> (String, B.ByteString)
parseSHLD (parsed, codes) =
  let c = B.head codes
  in case c of
       0x22      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "SHLD" register, B.drop 3 codes)

parseLHLD :: (String, B.ByteString) -> (String, B.ByteString)
parseLHLD (parsed, codes) =
  let c = B.head codes
  in case c of
       0x2a      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "LHLD" register, B.drop 3 codes)

parseSTA :: (String, B.ByteString) -> (String, B.ByteString)
parseSTA (parsed, codes) =
  let c = B.head codes
  in case c of
       0x32      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "STA" register, B.drop 3 codes)

parseLDA :: (String, B.ByteString) -> (String, B.ByteString)
parseLDA (parsed, codes) =
  let c = B.head codes
  in case c of
       0x3a      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "LDA" register, B.drop 3 codes)

parseJNZ :: (String, B.ByteString) -> (String, B.ByteString)
parseJNZ (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc2      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JNZ" register, B.drop 3 codes)

parseJMP :: (String, B.ByteString) -> (String, B.ByteString)
parseJMP (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc3      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JMP" register, B.drop 3 codes)

parseCNZ :: (String, B.ByteString) -> (String, B.ByteString)
parseCNZ (parsed, codes) =
  let c = B.head codes
  in case c of
       0xc4      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CNZ" register, B.drop 3 codes)

parseJZ :: (String, B.ByteString) -> (String, B.ByteString)
parseJZ (parsed, codes) =
  let c = B.head codes
  in case c of
       0xca      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JZ" register, B.drop 3 codes)

parseCZ :: (String, B.ByteString) -> (String, B.ByteString)
parseCZ (parsed, codes) =
  let c = B.head codes
  in case c of
       0xcc      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CZ" register, B.drop 3 codes)

parseCALL :: (String, B.ByteString) -> (String, B.ByteString)
parseCALL (parsed, codes) =
  let c = B.head codes
  in case c of
       0xcd      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CALL" register, B.drop 3 codes)

parseJNC :: (String, B.ByteString) -> (String, B.ByteString)
parseJNC (parsed, codes) =
  let c = B.head codes
  in case c of
       0xd2      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JNC" register, B.drop 3 codes)

parseCNC :: (String, B.ByteString) -> (String, B.ByteString)
parseCNC (parsed, codes) =
  let c = B.head codes
  in case c of
       0xd4      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CNC" register, B.drop 3 codes)

parseJC :: (String, B.ByteString) -> (String, B.ByteString)
parseJC (parsed, codes) =
  let c = B.head codes
  in case c of
       0xda      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JC" register, B.drop 3 codes)

parseCC :: (String, B.ByteString) -> (String, B.ByteString)
parseCC (parsed, codes) =
  let c = B.head codes
  in case c of
       0xdc      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CC" register, B.drop 3 codes)

parseSBI :: (String, B.ByteString) -> (String, B.ByteString)
parseSBI (parsed, codes) =
  let c = B.head codes
  in case c of
       0xde      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "SBI" register, B.drop 3 codes)

parseJPO :: (String, B.ByteString) -> (String, B.ByteString)
parseJPO (parsed, codes) =
  let c = B.head codes
  in case c of
       0xe2      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JPO" register, B.drop 3 codes)

parseCPO :: (String, B.ByteString) -> (String, B.ByteString)
parseCPO (parsed, codes) =
  let c = B.head codes
  in case c of
       0xe4      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CPO" register, B.drop 3 codes)

parseJPE :: (String, B.ByteString) -> (String, B.ByteString)
parseJPE (parsed, codes) =
  let c = B.head codes
  in case c of
       0xea      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JPE" register, B.drop 3 codes)

parseCPE :: (String, B.ByteString) -> (String, B.ByteString)
parseCPE (parsed, codes) =
  let c = B.head codes
  in case c of
       0xec      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CPE" register, B.drop 3 codes)

parseJP :: (String, B.ByteString) -> (String, B.ByteString)
parseJP (parsed, codes) =
  let c = B.head codes
  in case c of
       0xf2      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JP" register, B.drop 3 codes)

parseCP :: (String, B.ByteString) -> (String, B.ByteString)
parseCP (parsed, codes) =
  let c = B.head codes
  in case c of
       0xf4      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CP" register, B.drop 3 codes)

parseJM :: (String, B.ByteString) -> (String, B.ByteString)
parseJM (parsed, codes) =
  let c = B.head codes
  in case c of
       0xfa      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "JM" register, B.drop 3 codes)

parseCM :: (String, B.ByteString) -> (String, B.ByteString)
parseCM (parsed, codes) =
  let c = B.head codes
  in case c of
       0xfc      -> parse ("$" ++ (byteStringToStr $ B.reverse . B.take 2 . B.tail $ codes))
       otherwise -> (parsed, codes)
       where parse register = (parsed ++ parseSingIns "CM" register, B.drop 3 codes)
-- Operand and byte instructions
---------------------------
parseLXI :: (String, B.ByteString) -> (String, B.ByteString)
parseLXI (parsed, codes) =
  let c = B.head codes
  in case c of
       0x01 -> parse "B"
       0x11 -> parse "D"
       0x21 -> parse "H"
       0x31 -> parse "SP"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 2 (B.tail codes) "LXI" register, B.drop 3 codes)

parseMVI :: (String, B.ByteString) -> (String, B.ByteString)
parseMVI (parsed, codes) =
  let c = B.head codes
  in case c of
       0x06 -> parse "B"
       0x0e -> parse "C"
       0x16 -> parse "D"
       0x1e -> parse "E"
       0x26 -> parse "H"
       0x2e -> parse "L"
       0x36 -> parse "M"
       0x3e -> parse "A"
       otherwise  -> (parsed, codes)
       where parse register = (parsed ++ parseNumIns 1 (B.tail codes) "MVI" register, B.drop 2 codes)

