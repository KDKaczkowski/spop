
import System.IO
import System.IO.Error
import Control.Exception
import Data.List
import Data.Char
import System.Directory
import System.Environment
import CreekType
import Game
import PrintBoard


--main :: IO ()
main = do
    putStrLn "\n***************************************************"
    putStrLn "\n************** ŁAMIGŁÓWKA STRUMYK *****************"
    putStrLn "\n***************************************************"
    setCurrentDirectory "/home/kamil/Pobrane/spop-creek-master/test"
    testFiles <- filter isRegularFileOrDirectory <$> getDirectoryContents "."
    printTestFiles 0 testFiles
    putStrLn "Wybierz odpowiedni plik wpisując jego numer"
    fileNumber <- getLine
    let fileIndex = read fileNumber::Int
    let file_name = testFiles !! fileIndex
    putStrLn "\n***************************************************\n"
    readMyFile file_name
    return()

-- read file with board to solve
readMyFile :: String -> IO ()
readMyFile file_name =
  catch (do handle      <- openFile file_name ReadMode 
            file_content    <- hGetContents handle u
            let read_creek    = read file_content
            errorValue <- try (print read_creek) :: IO (Either SomeException ())
            putStrLn ("\n")
            case errorValue of
                Left  _    -> putStrLn "Niepoprawny format zawartości pliku."
                Right _  ->  doSolveGame read_creek
            hClose handle 
        ) errorHandler
        where
          errorHandler e =
            if ( openingError e )
            then putStrLn ("Nie udalo sie otworzyc pliku: " ++ file_name)
            else return ()

-- prints list of test files
printTestFiles :: Int -> [String] -> IO()
printTestFiles _ [] = return ()
printTestFiles n (x:xs) = do
                        putStrLn test
                        printTestFiles b xs
                        where test = show n ++ ". " ++ x
                              b = intIteration n

-- returns true if file have .txt extension
isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".." && isInfixOf ".txt" f

-- iteration function
intIteration :: Int -> Int
intIteration n = do n + 1

-- function which starts game solving
doSolveGame :: Creek -> IO ()
doSolveGame creek = do
    let size    = getBoardSize creek
        result     =  findCreek creek
    case (findCreek creek) of
        Left err -> putStrLn err
        Right result -> printBoard result size

-- error handling function
openingError :: IOError -> Bool
openingError e = isDoesNotExistError e || isAlreadyInUseError e 
                || isPermissionError e || isEOFError e


