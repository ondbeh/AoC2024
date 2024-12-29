-- Define imports
import Data.List (sort)

-- Parse a single line into a tuple of two integers
parseLine :: String -> (Int, Int)
parseLine line = case words line of
    [x, y] -> (read x, read y)
    _ -> error "Invalid input line format"

-- Separate the list of pairs into two lists
separateLists :: [(Int, Int)] -> ([Int], [Int])
separateLists pairs = unzip pairs

-- Calculate the distance between points in ordered lists
calculateOrderedDistance :: ([Int], [Int]) -> Int
calculateOrderedDistance (left, right) = 
    sum [abs (x - y) | (x, y) <- zip (sort left) (sort right)]

-- Count the number of occurences of an element in a list
countOccurences :: Int -> [Int] -> Int
countOccurences x = length . filter (== x)

-- Calculate the similarity score between two lists
calculateSimilarityScore :: ([Int], [Int]) -> Int
calculateSimilarityScore (left, right) = 
    sum [x * countOccurences x right | x <- left]

-- Main function to read and parse the file
main :: IO ()
main = do
    -- Read the contents of the file
    contents <- readFile "input.txt"
    -- Split into lines and parse each line
    let pairs = map parseLine (lines contents)
    -- Make a tuple of two lists from the pairs
    let lists = separateLists pairs
    -- Calculate the sum of distances between sorted lists
    let result1 = calculateOrderedDistance lists
    -- Print the result
    putStrLn $ "Result part 1: " ++ show result1
    -- Calculate the similarity score (number of occurences of each left element in right list * element)
    let result2 = calculateSimilarityScore lists
    -- Print the result
    putStrLn $ "Result part 2: " ++ show result2