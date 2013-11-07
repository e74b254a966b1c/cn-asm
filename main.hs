import System.Environment
import System.IO
import Control.Monad

data RAM = RAMMacro Int String | RAMValue Int Int deriving Show

data Instruction = Instr0Op { instr :: String } |
                   Instr1Op { instr :: String, op1 :: String } |
                   Instr2Op { instr :: String, op1 :: String, op2 :: String }
                   deriving Show

instr0Op = ["NOP", "HLT", ".*"]
instr1Op = ["GOTO", "JMPZ", "JMPNZ", "NOT", "NEG", "DB"]
instr2Op = ["LOAD", "STORE", "MOV", "AND", "OR", "XOR", "ADD", "SUB", "MUL"]

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

