import System.Environment
import System.IO
import Text.Regex
import Data.Maybe
import Control.Monad
import Data.Char (toUpper)

data RAM = RAMMacro Int String | RAMValue Int Int deriving Show

data Instruction = Instr0Op { instr :: String } |
                   Instr1Op { instr :: String, op1 :: String } |
                   Instr2Op { instr :: String, op1 :: String, op2 :: String }
                   deriving Show

instr0Op = map mkRegex ["NOP", "HLT", ".*"]
instr1Op = map mkRegex ["GOTO", "JMPZ", "JMPNZ", "NOT", "NEG", "DB"]
instr2Op = map mkRegex ["LOAD", "STORE", "MOV", "AND", "OR", "XOR", "ADD",
                        "SUB", "MUL"]

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
        mapM_ (hPutStrLn outHandle) (words contents)
        hClose outHandle

isInstr0Op word = null $ filter isJust $ map (\exp -> matchRegex exp word) instr0Op
isInstr1Op word = null $ filter isJust $ map (\exp -> matchRegex exp word) instr1Op
isInstr2Op word = null $ filter isJust $ map (\exp -> matchRegex exp word) instr2Op


getInstructions instructions [] = instructions
getInstructions instructions (word : words)
        | isInstr0Op wordUpr = getInstructions 
                                (Instr0Op {instr = wordUpr}
                                 : instructions) 
                                words
    where wordUpr = map toUpper word
getInstructions instructions (word1 : word2 : words)
        | isInstr1Op wordUpr = getInstructions 
                                (Instr1Op {instr = wordUpr,
                                           op1   = word2}
                                 : instructions) 
                                words
    where wordUpr = map toUpper word1
getInstructions instructions (word1 : word2 : word3 : words)
        | isInstr2Op wordUpr = getInstructions 
                                (Instr2Op {instr = wordUpr,
                                           op1 = word2,
                                           op2 = word3}
                                 : instructions) 
                                words
    where wordUpr = map toUpper word1

ramToString (RAMMacro addr val) = "ram[" ++ (show addr) ++ "]=" ++ val ++ ";"
ramToString (RAMValue addr val) = "ram[" ++ (show addr) ++ "]=" ++
                                   (show val)  ++ ";"

{-
main = do
        args <- getArgs  
        progName <- getProgName  
        putStrLn "The arguments are:"  
        mapM putStrLn args  
        putStrLn "The program name is:"  
        putStrLn progName  
        let list = []
        handle <- openFile "test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords
        hClose handle
-}

