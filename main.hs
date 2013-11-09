import System.Environment
import System.IO
import Text.Regex
import Data.Maybe
import Data.List
import Control.Monad
import Data.Word (Word8)
import Data.Char (toUpper)

data RAM = RAMMacro Int String | RAMValue Int Int deriving Show

data Instruction = Instr0Op { instr :: String } |
                   Instr1Op { instr :: String, op1 :: String } |
                   Instr2Op { instr :: String, op1 :: String, op2 :: String }
                   deriving Show

newtype Validated = Validated Instruction

data InstrAddr = InstrAddr {vInstr :: Validated, addr :: Int}

instr0Op = map mkRegex ["NOP", "HLT", "\\..*"]
instr1Op = map mkRegex ["GOTO", "JMPZ", "JMPNZ", "NOT", "NEG", "DB"]
instr2Op = map mkRegex ["LOAD", "STORE", "MOV", "AND", "OR", "XOR", "ADD",
                        "SUB", "MUL"]
regs = map mkRegex ["AX", "BX", "CX", "DX"]

main = do
        args <- getArgs
        progName <- getProgName
        if length args /= 2
        then do
            putStrLn "Wrong number of arguments. Usage:"
            putStrLn $ progName ++ " input_file output_file"
        else do
            putStrLn "cn-asm started..."
            transform (args !! 0) (args !! 1)
            

transform inFile outFile = do
        contents <- readFile inFile

        outHandle <- openFile outFile WriteMode
        mapM_ (hPutStrLn outHandle) (getCode (words contents))
        hClose outHandle

isInstr0Op word = not $ null $ filter isJust $ map (\exp -> matchRegex exp word) instr0Op
isInstr1Op word = not $ null $ filter isJust $ map (\exp -> matchRegex exp word) instr1Op
isInstr2Op word = not $ null $ filter isJust $ map (\exp -> matchRegex exp word) instr2Op
isReg word = not $ null $ filter isJust $ map (\exp -> matchRegex exp word) regs
isLabel word = isJust $ matchRegex (mkRegex "\\..*") word

transformToImm word
        | isLabel word = word
        | otherwise = show (read word::Word8)

getCode words = map ramToString $ concat $ reverse $
                    map instrToRam (replaceLabels instr (getLabelAddrList instr))
        where instr = getAddressedInstr words

replaceLabels instrAddr (x : labels) = replaceLabels (map (replaceLabelWithAddr x)
                                              (filter (not . labelFilter) instrAddr))
                                                labels
replaceLabels instrAddr [] = instrAddr

replaceLabelWithAddr _ (InstrAddr (Validated (Instr0Op x)) a) =
        InstrAddr (Validated (Instr0Op x)) a
replaceLabelWithAddr (label, ad) (InstrAddr (Validated (Instr1Op x o1)) a) =
    if label == o1 then InstrAddr (Validated (Instr1Op x ad)) a
        else InstrAddr (Validated (Instr1Op x o1)) a
replaceLabelWithAddr (label, ad) (InstrAddr (Validated (Instr2Op x o1 o2)) a)
    | label == o1 = 
                replaceLabelWithAddr (label, ad) (InstrAddr (Validated (Instr2Op x ad o2)) a)
    | label == o2 = 
                replaceLabelWithAddr (label, ad) (InstrAddr (Validated (Instr2Op x o1 ad)) a)
    | otherwise = InstrAddr (Validated (Instr2Op x o1 o2)) a

getLabelAddrList instrAddr = map instrAddrPair (filter labelFilter instrAddr)

labelFilter (InstrAddr (Validated (Instr0Op x)) _) = isLabel x
labelFilter _ = False

instrAddrPair x = (getInstr x, show $ getAddr x)

getAddr (InstrAddr _ x) = x
getInstr (InstrAddr (Validated (Instr0Op x)) _) = x;
getInstr (InstrAddr (Validated (Instr1Op x _)) _) = x;
getInstr (InstrAddr (Validated (Instr2Op x _ _)) _) = x;

getAddressedInstr words = putAddrToInstr (getValidatedInstructions words)
                                         0
                                         []

putAddrToInstr [] _ instrOut = instrOut
putAddrToInstr (ins : instrIn) startAddr instrOut =
    putAddrToInstr instrIn (startAddr + numberOfBytes ins)
         (InstrAddr {vInstr = ins, addr = startAddr} : instrOut)

numberOfBytes (Validated (Instr0Op x))
        | isLabel x = 0
        | otherwise = 1
numberOfBytes (Validated (Instr1Op x _)) 
        | x == "DB" = 1
        | otherwise = 2
numberOfBytes (Validated (Instr2Op _ _ _)) = 3

getValidatedInstructions words = reverse $ map validateInstr
                                               (getInstructions [] words)
validateInstr Instr0Op {instr = x} = Validated Instr0Op {instr = x}
validateInstr Instr1Op {instr = x,
                        op1 = o1}
        | x == "NOT" || x == "NEG" = if isReg oUppr
                                        then Validated Instr1Op {instr = x,
                                                                 op1 = oUppr}
                                        else error $ "Invalid 1OP instruction\n" ++
                                                     "Expecting register"
        | otherwise = Validated Instr1Op {instr = x,
                                          op1 = transformToImm oUppr}
        where oUppr = toUpperString o1

validateInstr Instr2Op {instr = x,
                        op1 = o1,
                        op2 = o2}
        | x == "LOAD" = if isReg o1Uppr
                            then Validated Instr2Op {instr = x,
                                                     op1 = o1Uppr,
                                                     op2 = 
                                                         transformToImm o2Uppr}
                            else error $ "Invalid 2OP instruction\n" ++
                                         "Expecting first argument to be REG"
        | x == "STORE" = if isReg o2Uppr
                            then Validated Instr2Op {instr = x,
                                                     op1 = 
                                                         transformToImm o1Uppr,
                                                     op2 = o2Uppr}
                            else error $ "Invalid 2OP instruction\n" ++
                                        "Expecting second argument to be REG"
        | otherwise = if isReg o1Uppr && isReg o2Uppr
                            then Validated Instr2Op {instr = x,
                                                     op1 = o1Uppr,
                                                     op2 = o2Uppr}
                            else error $ "Invalid 2OP instruction\n" ++
                                         "Expecting both arguments to be REGS"
        where o1Uppr = toUpperString o1 
              o2Uppr = toUpperString o2

getInstructions :: [Instruction] -> [String] -> [Instruction]
getInstructions instructions [] = instructions
getInstructions instructions (word : words)
        | isInstr0Op wordUpr = getInstructions 
                                (Instr0Op {instr = wordUpr}
                                 : instructions) 
                                words
    where wordUpr = toUpperString word
getInstructions instructions (word1 : word2 : words)
        | isInstr1Op wordUpr = getInstructions 
                                (Instr1Op {instr = wordUpr,
                                           op1   = word2}
                                 : instructions) 
                                words
    where wordUpr = toUpperString word1
getInstructions instructions (word1 : word2 : word3 : words)
        | isInstr2Op wordUpr = getInstructions 
                                (Instr2Op {instr = wordUpr,
                                           op1 = word2,
                                           op2 = word3}
                                 : instructions) 
                                words
    where wordUpr = toUpperString word1

toUpperString :: [Char] -> [Char]
toUpperString s = map toUpper s

instrToRam (InstrAddr (Validated (Instr0Op x)) a) = [RAMMacro a x]
instrToRam (InstrAddr (Validated (Instr1Op x o1)) a)
    | x == "NOT" || x == "NEG" = [RAMMacro a x, RAMMacro (a + 1) o1]
    | x == "DB" = [RAMMacro a o1]
    | otherwise = [RAMMacro a x, RAMValue (a + 1) (read o1::Int)]
instrToRam (InstrAddr (Validated (Instr2Op x o1 o2)) a)
    | x == "LOAD" = [RAMMacro a x, RAMMacro (a + 1) o1,
                     RAMValue (a + 2) (read o2::Int)]
    | x == "STORE" = [RAMMacro a x, RAMValue (a + 1) (read o1::Int),
                      RAMMacro (a + 2) o2]
    | otherwise = [RAMMacro a x, RAMMacro (a + 1) o1, RAMMacro (a + 2) o2]

ramToString (RAMMacro addr val) = "ram[" ++ (show addr) ++ "]=`" ++ val ++ ";\n"
ramToString (RAMValue addr val) = "ram[" ++ (show addr) ++ "]=" ++
                                   (show val)  ++ ";\n"


