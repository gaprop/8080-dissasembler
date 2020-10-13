import Data.Char
import Data.Bits
import Numeric (showHex)
import System.Environment
import GHC.Int
import GHC.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import InstructionSet

main = do
  args    <- getArgs
  content <- B.readFile (args !! 0)
  putStr . disassemble $ content
  
disassemble :: B.ByteString -> String
disassemble codes = disassemble' ("", codes)
  where disassemble' :: (String, B.ByteString) -> String
        disassemble' (str, codes) =
          if codes == B.empty then str
          else
            let c = (flip Map.lookup instructions) . B.head $ codes
            in disassemble' $ parse str (B.tail codes) c

parse :: String -> B.ByteString -> Maybe InsType -> (String, B.ByteString)
parse parsed codes Nothing = (parsed ++ "NOP\n", codes)
parse parsed codes (Just c) =
  case c of
    NoneOperand _ -> noneOperandParse parsed codes c
    OneOperand _ _ -> oneOperandParse parsed codes c
    TwoOperand _ _ _ -> twoOperandParse parsed codes c
    OneByte _ -> oneByteParse parsed codes c
    OnAdress _ -> onAdressParse parsed codes c
    OperandAndByte _ _ -> operandAndByteParse parsed codes c
    NumberOperand _ _ -> numberOperandParse parsed codes c

-- Everything below is complete dogshit
----------------------------------------
noneOperandParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
noneOperandParse parsed codes (NoneOperand ins) = 
  (parsed ++ ins ++ "\n", codes)

twoOperandParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
twoOperandParse parsed codes (TwoOperand ins op1 op2) = 
  (parsed ++ ins ++ " " ++ op1 ++ ", " ++ op2 ++ "\n", codes)

oneOperandParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
oneOperandParse parsed codes (OneOperand ins op) = 
  (parsed ++ ins ++ " " ++ op ++ "\n", codes)

oneByteParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
oneByteParse parsed codes (OneByte ins) = 
  (parsed ++ ins ++ " " ++ (byteStringToStr $ B.take 1 codes) ++ "\n", B.tail codes)

onAdressParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
onAdressParse parsed codes (OnAdress ins) = 
  (parsed ++ ins ++ " " ++ "$" ++ (byteStringToStr $ B.reverse . B.take 2 $ codes) ++ "\n", B.drop 2 codes)

operandAndByteParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
operandAndByteParse parsed codes (OperandAndByte ins op) = 
  (parsed ++ ins ++ " " ++ reg ++ ", " ++ (byteStringToStr $ B.take 1 codes) ++ "\n", B.tail codes)

numberOperandParse :: String -> B.ByteString -> InsType -> (String, B.ByteString)
numberOperandParse parsed codes (NumberOperand ins op) = 
  (parsed ++ ins ++ " " ++ op ++ "\n", codes)

-- NOTE: Proly a really dumb way of converting byte string to string
byteStringToStr :: B.ByteString -> String
byteStringToStr = concat . map (toHex . read . show) . B.unpack

toHex :: Int -> String
toHex a
  | a < 16    = "0" ++ showHex a ""
  | otherwise = showHex a ""

