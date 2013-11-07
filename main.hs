import System.Environment
import System.IO
import Control.Monad

main = do
        args <- getArgs
        progName <- getProgName
        if length args /= 2
        then do
            putStrLn "Wrong number of arguments. Usage:"
            putStrLn $ progName ++ " input_file output_file"
        else do
            putStrLn "ok"


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

