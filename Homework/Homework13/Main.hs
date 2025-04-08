import System.Directory (listDirectory)
--import Data.List (isInfixOf, sort) -- import only useful functions. It avoids polluting our program with function names we won't use.
import Data.List hiding (find)
import qualified Data.Map -- conflit de noms entre 2 imports : qualified permet de forcer à spécifier Data.map.filter pour résoudre le conflit

-- >>> listDirectory "."

find :: String -> IO (Data.Map.Map Int String)
find searchTerm = do
    entries <- listDirectory "."
    let found = sort $ filter (searchTerm `isInfixOf`) entries
    let foundMap = Data.Map.fromList $ zip ([1..] :: [Int]) found
    return foundMap

main = do
    putStrLn "Provide search term: "
    searchTerm <- getLine
    found <- find searchTerm
    putStrLn $ "These entries match your search: " ++ show found

