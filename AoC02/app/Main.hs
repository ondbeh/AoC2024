-- Includes
import Text.Read (readMaybe)  -- For safe parsing of integers
import Data.Maybe (mapMaybe)  -- For handling Maybe values

-- Parse a single line into a list of integers safely
parseLine :: String -> Maybe [Int]
parseLine = mapM readMaybe . words

-- Check if the list is ordered and safe
isOrderedAndSafe :: (Int -> Int -> Bool) -> [Int] -> Bool
isOrderedAndSafe cmp xs 
    | length xs < 2 = True  -- Handle empty or single-element lists
    | otherwise = all checkPair pairs
  where 
    pairs = zip xs (tail xs)
    checkPair (x, y) = cmp x y && diff <= 3
      where diff = abs (x - y)

-- Check if the list is ordered
isSafe :: [Int] -> Bool
isSafe line = isOrderedAndSafe (<) line || isOrderedAndSafe (>) line

main :: IO ()
main = do
    -- Read the contents of the file
    contents <- readFile "input.txt"
    -- Parse the lines safely and filter out the invalid ones
    let reports = mapMaybe parseLine $ lines contents
    -- Filter the lines to find only ordered lines and count them
    let result1 = length $ filter isSafe reports
    -- Print the result
    putStrLn $ "Result part 1: " ++ show result1