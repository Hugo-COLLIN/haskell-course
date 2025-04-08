import System.Directory (listDirectory)
--import Data.List (isInfixOf, sort) -- import only useful functions. It avoids polluting our program with function names we won't use.
import Data.List hiding (find)

-- conflit de noms entre 2 imports : qualified permet de forcer à spécifier Data.map.filter pour résoudre le conflit
import qualified Data.Map as Map hiding (Map)
import Data.Map (Map) -- avoids to write Map.Map

-- >>> listDirectory "."

find :: String -> IO (Map Int String)
find searchTerm = do
    entries <- listDirectory "."
    let found = sort $ filter (searchTerm `isInfixOf`) entries
    let foundMap = Map.fromList $ zip ([1..] :: [Int]) found
    return foundMap

main = do
    putStrLn "Provide search term: "
    searchTerm <- getLine
    found <- find searchTerm
    putStrLn $ "These entries match your search: " ++ show found

