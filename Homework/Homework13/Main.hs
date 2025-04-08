import System.Directory
import Data.List

-- >>> listDirectory "."

find' :: String -> IO [FilePath]
find' searchTerm = do
    entries <- listDirectory "."
    let found = filter (searchTerm `isInfixOf`) entries
    return found

main = do
    putStrLn "Provide search term: "
    searchTerm <- getLine
    found <- find' searchTerm
    putStrLn $ "These entries match your search: " ++ show found

