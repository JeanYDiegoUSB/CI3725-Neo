import LexNeo
import SintNeo
import System.Environment

-- show for List of Token
myShow :: [Token] -> String
myShow [] = ""
myShow (x:xs) = show x ++ "\n" ++ myShow xs

-- print for List of Token
myPrint :: [Token] -> IO()
myPrint a = putStr $ myShow a

-- print para S
printS :: S -> IO()
printS a = putStr $ show a

-- Main program
main :: IO()
main = do
    args <- getArgs -- command line arguments
    let handle = head args -- first argument (file.neo)
    s <- readFile handle -- reading the file 
    let tokens = alexScanTokens s -- List of Token
    let result = filter f tokens -- courtesy of Ricardo Monascal
                    where
                        f :: Token -> Bool
                        f (TkError _ _) = True
                        f _ = False
    if result /= []
        then myPrint result
        else printS $ parse tokens
