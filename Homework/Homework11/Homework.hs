import Data.List
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, listDirectory)
import Text.XHtml (thead)
import Control.DeepSeq


{-
We imported some functions that you'll need to complete the homework.
FilePath is just a synonym for String. Although, make sure to follow the standard path
representation when using them (https://en.wikipedia.org/wiki/Path_(computing).
getCPUTime    :: IO Integer
doesFileExist :: FilePath -> IO Bool
listDirectory :: FilePath -> IO [FilePath]
You can hover over the functions to know what they do.
-}

{-
-- Question 1 --
Define an IO action that counts the number of all enteries in the current directory
and prints it to the terminal inside a string message.
(hidden files are not included)
-}

listFiles :: IO ()
listFiles = do
    entries <- listDirectory "."

    let visibleEntries = filter (not . isPrefixOf ".") entries

    putStrLn $ "Visible entries: " ++ show (length visibleEntries)


{-
-- Question 2 --
Write an IO action that asks the user to type something, then writes the message
to a file called msg.txt, and after that, it reads the text from the msg.txt
file and prints it back. Use the writeFile and readFile functions.
-}

createMsg :: IO ()
createMsg = do
    putStrLn "Please type something (anything please!!!) :"
    msg <- getLine
    writeFile "msg.txt" msg

    content <- readFile "msg.txt"
    putStrLn ("Content of msg.txt:\n" ++ content)



{-
-- Context for Questions 3 and 4 --
In cryptography, prime numbers (positive integers only divisible by themselves and 1) play a fundamental
role in providing unbreakable mathematical structures. These structures, in turn, are leveraged to
establish secure encryption.
But, generating primes is a computational straining problem, as we will measure in the following exercise.
This is because, to know whether a number is a prime number, you first need to know all the previous primes
and then check that they are not a divisor of this number. So, this problem gets bigger and bigger!
Our lead cryptographer provided us with 3 different algorithms (primes1, primes2, and primes3). All three
correctly produce a list of all the prime numbers until a limit (that we provide as a parameter).
Our job is not to understand these algorithms but to measure which is the fastest and print the largest
prime number below our limit. Do it step by step, starting with question 3.
-}

primes1 :: Integer -> [Integer]
primes1 m = sieve [2 .. m]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2 .. m]
 where
  sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
  sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2 .. m]
 where
  turner [] = []
  turner (p : xs) = p : turner [x | x <- xs, x < p * p || rem x p /= 0]

{-
-- Question 3 --
Define an IO action that takes an IO action as input and calculates the time it takes to execute.
Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
The CPU time here is given in picoseconds (which is 1/1000000000000th of a second).
-}

type TimedResult a = (Double, a)

timeIO :: NFData a => IO a -> IO (TimedResult a)
timeIO action = do
    -- Get initial CPU time
    startTime <- getCPUTime

    -- Execute the action and get its result
    result <- action

    -- Important : forcer l'évaluation avant de mesurer le temps final
    result `deepseq` return () -- Cette ligne est cruciale

    -- Get final CPU time
    endTime <- getCPUTime

    -- Calculate elapsed time in seconds
    let elapsedTime = fromInteger (endTime - startTime) / 1e12

    return (elapsedTime, result)

-- Fonction utilitaire pour convertir une fonction pure en action IO
testPrimeFunction :: NFData a => (a -> [a]) -> a -> IO (TimedResult [a])
testPrimeFunction primeFunc n = timeIO (return $ primeFunc n)

{-
-- Question 4 --
Write an action that retrieves a value from the standard input, parses it as an integer,
and compares the time all three algorithms take to produce the largest prime before the
limit. Print the number and time to the standard output.
-}

benchmark :: IO ()
benchmark = do
    putStr "Enter a value param: "
    param <- getLine
    let paramInt = read param :: Integer

    (t1, l1) <- testPrimeFunction primes1 paramInt
    putStrLn $ "Primes1: " ++ show t1 ++ "s with " ++ show (last l1)

    (t2, l2) <- testPrimeFunction primes2 paramInt
    putStrLn $ "Primes2: " ++ show t2 ++ "s with " ++ show (last l1)

    (t3, l3) <- testPrimeFunction primes3 paramInt
    putStrLn $ "Primes3: " ++ show t3 ++ "s with " ++ show (last l1)




{-
 -- Question 5 -- EXTRA CREDITS -- (In case the previous ones were too easy)
Write a program that prints the directory tree structure from the current folder.
Below you can see an example output of how such a structure looks like:
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: You can use the function doesFileExist, which takes in a FilePath and returns
True if the argument file exists and is not a directory, and False otherwise.
-}
