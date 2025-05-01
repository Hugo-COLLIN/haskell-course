{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import Control.Exception (try, IOException)
import Data.Char (isDigit, isUpper, isLower)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : l) = (a : catMaybes l)
catMaybes (Nothing : l) = catMaybes l


-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> return Nothing
        Right content -> return (Just content)

-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError = 
      TooShort Int      -- Current length and minimum required
    | NoDigit
    | NoUpperCase
    | NoLowerCase
    deriving Show

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough pwd
    | length pwd >= 10 = Right pwd
    | otherwise = Left (TooShort (length pwd))

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit pwd
    | any isDigit pwd = Right pwd
    | otherwise = Left NoDigit


passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase pwd
    | any isUpper pwd = Right pwd
    | otherwise = Left NoUpperCase

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase pwd
    | any isLower pwd = Right pwd
    | otherwise = Left NoLowerCase

passwordRequirements :: String -> Either PasswordError String
-- passwordRequirements pwd = do
--     p1 <- passwordLongEnough pwd
--     p2 <- passwordHasDigit p1
--     p3 <- passwordHasUppercase p2
--     passwordHasLowercase p3

passwordRequirements pwd = 
    passwordLongEnough pwd >>= passwordHasDigit >>= passwordHasUppercase >>= passwordHasLowercase
