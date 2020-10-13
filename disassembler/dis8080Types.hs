module InstructionSet (instructions, InsType(..)) where

import qualified Data.Map as Map
import GHC.Word

type Operand = String
type HexString = String -- Not to be confused with a byte string
type Instruction = String

data InsType = NoneOperand Instruction
             | OneOperand Instruction Operand 
             | TwoOperand Instruction Operand Operand
             | OneByte Instruction
             | OnAdress Instruction
             | OperandAndByte Instruction Operand
             | NumberOperand Instruction Operand
             deriving (Show)

instructions :: Map.Map Word8 InsType
instructions = Map.fromList [
                (0x00, NoneOperand "NOP")
              , (0x08, NoneOperand "NOP")
              , (0x10, NoneOperand "NOP")
              , (0x18, NoneOperand "NOP")
              , (0x20, NoneOperand "NOP")
              , (0x28, NoneOperand "NOP")
              , (0x30, NoneOperand "NOP")
              , (0x38, NoneOperand "NOP")
              , (0xcb, NoneOperand "NOP")
              , (0xd9, NoneOperand "NOP")
              , (0xdd, NoneOperand "NOP")
              , (0xed, NoneOperand "NOP")
              , (0xfd, NoneOperand "NOP")
              , (0x07, NoneOperand "RLC")
              , (0x0f, NoneOperand "RRC")
              , (0x17, NoneOperand "RAL")
              , (0x1f, NoneOperand "RAR")
              , (0x27, NoneOperand "DAA")
              , (0x2f, NoneOperand "CMA")
              , (0x37, NoneOperand "STC")
              , (0x3f, NoneOperand "CMC")
              , (0x76, NoneOperand "HLT")
              , (0xc0, NoneOperand "RNZ")
              , (0xc8, NoneOperand "RZ")
              , (0xc9, NoneOperand "RET")
              , (0xd0, NoneOperand "RNC")
              , (0xd8, NoneOperand "RC")
              , (0xe0, NoneOperand "RPO")
              , (0xe3, NoneOperand "XTHL")
              , (0xe8, NoneOperand "RPE")
              , (0xe9, NoneOperand "PCHL")
              , (0xeb, NoneOperand "XCHG")
              , (0xf0, NoneOperand "RP")
              , (0xf3, NoneOperand "DI")
              , (0xf8, NoneOperand "RM")
              , (0xf9, NoneOperand "SPHL")
              , (0xfb, NoneOperand "EI")
              , (0x02, OneOperand "STAX" "B")
              , (0x12, OneOperand "STAX" "D")
              , (0x03, OneOperand "INX" "B" )
              , (0x13, OneOperand "INX" "D" )
              , (0x23, OneOperand "INX" "H" )
              , (0x33, OneOperand "INX" "SP")
              , (0x04, OneOperand "INR" "B" )
              , (0x0c, OneOperand "INR" "C" )
              , (0x14, OneOperand "INR" "D" )
              , (0x1c, OneOperand "INR" "E" )
              , (0x24, OneOperand "INR" "H" )
              , (0x2c, OneOperand "INR" "L" )
              , (0x34, OneOperand "INR" "M" )
              , (0x3c, OneOperand "INR" "A" )
              , (0x05, OneOperand "DCR" "B" )
              , (0x0d, OneOperand "DCR" "C" )
              , (0x15, OneOperand "DCR" "D" )
              , (0x1d, OneOperand "DCR" "E" )
              , (0x25, OneOperand "DCR" "H" )
              , (0x2d, OneOperand "DCR" "L" )
              , (0x35, OneOperand "DCR" "M" )
              , (0x3d, OneOperand "DCR" "A" )
              , (0x09, OneOperand "DAD" "B" )
              , (0x19, OneOperand "DAD" "D" )
              , (0x29, OneOperand "DAD" "H" )
              , (0x39, OneOperand "DAD" "SP")
              , (0x0a, OneOperand "LDAX" "B")
              , (0x1a, OneOperand "LDAX" "D")
              , (0x0b, OneOperand "DCX" "B" )
              , (0x1b, OneOperand "DCX" "D" )
              , (0x2b, OneOperand "DCX" "H" )
              , (0x3b, OneOperand "DCX" "SP")
              , (0xc1, OneOperand "POP" "B" )
              , (0xd1, OneOperand "POP" "D" )
              , (0xe1, OneOperand "POP" "H" )
              , (0xf1, OneOperand "POP" "SP")
              , (0xc5, OneOperand "PUSH" "B" )
              , (0xd5, OneOperand "PUSH" "D" )
              , (0xe5, OneOperand "PUSH" "H" )
              , (0xf5, OneOperand "PUSH" "SP")
              , (0x80, OneOperand "ADD" "B")
              , (0x81, OneOperand "ADD" "C")
              , (0x82, OneOperand "ADD" "D")
              , (0x83, OneOperand "ADD" "E")
              , (0x84, OneOperand "ADD" "H")
              , (0x85, OneOperand "ADD" "L")
              , (0x86, OneOperand "ADD" "M")
              , (0x87, OneOperand "ADD" "A")
              , (0x88, OneOperand "ADC" "B")
              , (0x89, OneOperand "ADC" "C")
              , (0x8a, OneOperand "ADC" "D")
              , (0x8b, OneOperand "ADC" "E")
              , (0x8c, OneOperand "ADC" "H")
              , (0x8d, OneOperand "ADC" "L")
              , (0x8e, OneOperand "ADC" "M")
              , (0x8f, OneOperand "ADC" "A")
              , (0x90, OneOperand "SUB" "B")
              , (0x91, OneOperand "SUB" "C")
              , (0x92, OneOperand "SUB" "D")
              , (0x93, OneOperand "SUB" "E")
              , (0x94, OneOperand "SUB" "H")
              , (0x95, OneOperand "SUB" "L")
              , (0x96, OneOperand "SUB" "M")
              , (0x97, OneOperand "SUB" "A")
              , (0x98, OneOperand "SBB" "B")
              , (0x99, OneOperand "SBB" "C")
              , (0x9a, OneOperand "SBB" "D")
              , (0x9b, OneOperand "SBB" "E")
              , (0x9c, OneOperand "SBB" "H")
              , (0x9d, OneOperand "SBB" "L")
              , (0x9e, OneOperand "SBB" "M")
              , (0x9f, OneOperand "SBB" "A")
              , (0xa0, OneOperand "ANA" "B")
              , (0xa1, OneOperand "ANA" "C")
              , (0xa2, OneOperand "ANA" "D")
              , (0xa3, OneOperand "ANA" "E")
              , (0xa4, OneOperand "ANA" "H")
              , (0xa5, OneOperand "ANA" "L")
              , (0xa6, OneOperand "ANA" "M")
              , (0xa7, OneOperand "ANA" "A")
              , (0xa8, OneOperand "XRA" "B")
              , (0xa9, OneOperand "XRA" "C")
              , (0xaa, OneOperand "XRA" "D")
              , (0xab, OneOperand "XRA" "E")
              , (0xac, OneOperand "XRA" "H")
              , (0xad, OneOperand "XRA" "L")
              , (0xae, OneOperand "XRA" "M")
              , (0xaf, OneOperand "XRA" "A")
              , (0xb0, OneOperand "ORA" "B")
              , (0xb1, OneOperand "ORA" "C")
              , (0xb2, OneOperand "ORA" "D")
              , (0xb3, OneOperand "ORA" "E")
              , (0xb4, OneOperand "ORA" "H")
              , (0xb5, OneOperand "ORA" "L")
              , (0xb6, OneOperand "ORA" "M")
              , (0xb7, OneOperand "ORA" "A")
              , (0xb8, OneOperand "CMP" "B")
              , (0xb9, OneOperand "CMP" "C")
              , (0xba, OneOperand "CMP" "D")
              , (0xbb, OneOperand "CMP" "E")
              , (0xbc, OneOperand "CMP" "H")
              , (0xbd, OneOperand "CMP" "L")
              , (0xbe, OneOperand "CMP" "M")
              , (0xbf, OneOperand "CMP" "A")
              , (0xce, OneByte "ACI")
              , (0xd3, OneByte "OUT")
              , (0xc6, OneByte "ADI")
              , (0xd6, OneByte "SUI")
              , (0xdb, OneByte "IN" )
              , (0xe6, OneByte "ANI")
              , (0xee, OneByte "XRI")
              , (0xf6, OneByte "ORI")
              , (0xfe, OneByte "CPI")
              , (0x22, OnAdress "SHLD")
              , (0x2a, OnAdress "LHLD")
              , (0x32, OnAdress "STA" )
              , (0x3a, OnAdress "LDA" )
              , (0xc2, OnAdress "JNZ" )
              , (0xc3, OnAdress "JMP" )
              , (0xc4, OnAdress "CNZ" )
              , (0xca, OnAdress "JZ"  )
              , (0xcc, OnAdress "CZ"  )
              , (0xcd, OnAdress "CALL")
              , (0xd2, OnAdress "JNC" )
              , (0xd4, OnAdress "CNC" )
              , (0xda, OnAdress "JC"  )
              , (0xdc, OnAdress "CC"  )
              , (0xde, OnAdress "SBI" )
              , (0xe2, OnAdress "JPO" )
              , (0xe4, OnAdress "CPO" )
              , (0xea, OnAdress "JPE" )
              , (0xec, OnAdress "CPE" )
              , (0xf2, OnAdress "JP"  )
              , (0xf4, OnAdress "CP"  )
              , (0xfa, OnAdress "JM"  )
              , (0xfc, OnAdress "CM"  )
              , (0x01, OperandAndByte "LXI" "B" )
              , (0x11, OperandAndByte "LXI" "D" )
              , (0x21, OperandAndByte "LXI" "H" )
              , (0x31, OperandAndByte "LXI" "SP")
              , (0x06, OperandAndByte "MVI" "B" )
              , (0x0e, OperandAndByte "MVI" "C" )
              , (0x16, OperandAndByte "MVI" "D" )
              , (0x1e, OperandAndByte "MVI" "E" )
              , (0x26, OperandAndByte "MVI" "H" )
              , (0x2e, OperandAndByte "MVI" "L" )
              , (0x36, OperandAndByte "MVI" "M" )
              , (0x3e, OperandAndByte "MVI" "A" )
              , (0xc7, NumberOperand "RST" "0")
              , (0xcf, NumberOperand "RST" "1")
              , (0xd7, NumberOperand "RST" "2")
              , (0xdf, NumberOperand "RST" "3")
              , (0xe7, NumberOperand "RST" "4")
              , (0xef, NumberOperand "RST" "5")
              , (0xf7, NumberOperand "RST" "6")
              , (0xff, NumberOperand "RST" "7")
              , (0x40, TwoOperand "MOV" "B" "B")
              , (0x41, TwoOperand "MOV" "B" "C")
              , (0x42, TwoOperand "MOV" "B" "D")
              , (0x43, TwoOperand "MOV" "B" "E")
              , (0x44, TwoOperand "MOV" "B" "H")
              , (0x45, TwoOperand "MOV" "B" "L")
              , (0x46, TwoOperand "MOV" "B" "M")
              , (0x47, TwoOperand "MOV" "B" "A")
              , (0x48, TwoOperand "MOV" "C" "B")
              , (0x49, TwoOperand "MOV" "C" "C")
              , (0x4a, TwoOperand "MOV" "C" "D")
              , (0x4b, TwoOperand "MOV" "C" "E")
              , (0x4c, TwoOperand "MOV" "C" "H")
              , (0x4d, TwoOperand "MOV" "C" "L")
              , (0x4e, TwoOperand "MOV" "C" "M")
              , (0x4f, TwoOperand "MOV" "C" "A")
              , (0x50, TwoOperand "MOV" "D" "B")
              , (0x51, TwoOperand "MOV" "D" "C")
              , (0x52, TwoOperand "MOV" "D" "D")
              , (0x53, TwoOperand "MOV" "D" "E")
              , (0x54, TwoOperand "MOV" "D" "H")
              , (0x55, TwoOperand "MOV" "D" "L")
              , (0x56, TwoOperand "MOV" "D" "M")
              , (0x57, TwoOperand "MOV" "D" "A")
              , (0x58, TwoOperand "MOV" "E" "B")
              , (0x59, TwoOperand "MOV" "E" "C")
              , (0x5a, TwoOperand "MOV" "E" "D")
              , (0x5b, TwoOperand "MOV" "E" "E")
              , (0x5c, TwoOperand "MOV" "E" "H")
              , (0x5d, TwoOperand "MOV" "E" "L")
              , (0x5e, TwoOperand "MOV" "E" "M")
              , (0x5f, TwoOperand "MOV" "E" "A")
              , (0x60, TwoOperand "MOV" "H" "B")
              , (0x61, TwoOperand "MOV" "H" "C")
              , (0x62, TwoOperand "MOV" "H" "D")
              , (0x63, TwoOperand "MOV" "H" "E")
              , (0x64, TwoOperand "MOV" "H" "H")
              , (0x65, TwoOperand "MOV" "H" "L")
              , (0x66, TwoOperand "MOV" "H" "M")
              , (0x67, TwoOperand "MOV" "H" "A")
              , (0x68, TwoOperand "MOV" "L" "B")
              , (0x69, TwoOperand "MOV" "L" "C")
              , (0x6a, TwoOperand "MOV" "L" "D")
              , (0x6b, TwoOperand "MOV" "L" "E")
              , (0x6c, TwoOperand "MOV" "L" "H")
              , (0x6d, TwoOperand "MOV" "L" "L")
              , (0x6e, TwoOperand "MOV" "L" "M")
              , (0x6f, TwoOperand "MOV" "L" "A")
              , (0x70, TwoOperand "MOV" "M" "B")
              , (0x71, TwoOperand "MOV" "M" "C")
              , (0x72, TwoOperand "MOV" "M" "D")
              , (0x73, TwoOperand "MOV" "M" "E")
              , (0x74, TwoOperand "MOV" "M" "H")
              , (0x75, TwoOperand "MOV" "M" "L")
              , (0x77, TwoOperand "MOV" "M" "A")
              , (0x78, TwoOperand "MOV" "A" "B")
              , (0x79, TwoOperand "MOV" "A" "C")
              , (0x7a, TwoOperand "MOV" "A" "D")
              , (0x7b, TwoOperand "MOV" "A" "E")
              , (0x7c, TwoOperand "MOV" "A" "H")
              , (0x7d, TwoOperand "MOV" "A" "L")
              , (0x7e, TwoOperand "MOV" "A" "M")
              , (0x7f, TwoOperand "MOV" "A" "A")
             ]