module Main where

import Text.Read (readMaybe)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- This is a CLI application that allows the user to manage a TODO list.
-- The user can add and remove items from the list and save and load the list
-- from a file.
-- It's a working prototype, but it has some bugs. Specifically, it doesn't
-- handle errors very well. Your mission, should you choose to accept it, is to
-- fix those bugs and make the application more robust. Hint: Try to interact
-- with the application in unexpected ways and see what happens! You should be
-- able to find and fix 3 bugs.

printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

-- Bug Fix 1: Safe delete function that checks index bounds
delete :: Int -> [a] -> Maybe [a]
delete _ [] = Nothing
delete n xs
    | n < 0 || n >= length xs = Nothing
    | otherwise = Just $ take n xs ++ drop (n+1) xs

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()
  ('+' : ' ' : todo) -> prompt (todo : todos)

  -- Bug Fix 1: Safe deletion with bounds checking and validation
  ('-' : ' ' : num) -> case readMaybe num of
    Nothing -> do
      putStrLn $ "Error: '" ++ num ++ "' is not a valid number."
      prompt todos
    Just n -> case delete n todos of
      Nothing -> do
        putStrLn $ "Error: Item " ++ show n ++ " doesn't exist."
        prompt todos
      Just newTodos -> prompt newTodos
  ('s' : ' ' : fn) ->
    writeFile fn (show todos)
  ('l' : ' ' : fn) -> readFile fn >>= prompt . read
  _ -> do
    putStrLn ("Invalid command: `" ++ cmd ++ "`")
    prompt todos

printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn "+ <Item Name>   - Add a TODO entry"
  putStrLn "- <Item Number> - Delete the numbered entry"
  putStrLn "s <File Name>   - Save the current list of TODOs"
  putStrLn "l <File Name>   - Load the saved list of TODOs"
  putStrLn "q               - Quit without saving"

main :: IO ()
main = do
  printCommands
  prompt []
