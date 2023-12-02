import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

calibrationValue :: [Int] -> Int
calibrationValue xs = head xs * 10 + last xs

digitify :: String -> Maybe Int
digitify s
    | "one" `isPrefixOf` s = Just 1
    | "1" `isPrefixOf` s = Just 1
    | "two" `isPrefixOf` s = Just 2
    | "2" `isPrefixOf` s = Just 2
    | "three" `isPrefixOf` s = Just 3
    | "3" `isPrefixOf` s = Just 3
    | "four" `isPrefixOf` s = Just 4
    | "4" `isPrefixOf` s = Just 4
    | "five" `isPrefixOf` s = Just 5
    | "5" `isPrefixOf` s = Just 5
    | "six" `isPrefixOf` s = Just 6
    | "6" `isPrefixOf` s = Just 6
    | "seven" `isPrefixOf` s = Just 7
    | "7" `isPrefixOf` s = Just 7
    | "eight" `isPrefixOf` s = Just 8
    | "8" `isPrefixOf` s = Just 8
    | "nine" `isPrefixOf` s = Just 9
    | "9" `isPrefixOf` s = Just 9
    | otherwise = Nothing

part1 :: [String] -> Int
part1 = sum . map (calibrationValue . map digitToInt . filter isDigit)

part2 :: [String] -> Int
part2 = sum . map (calibrationValue . mapMaybe digitify . tails)

main :: IO ()
main = do
    l <- lines <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 l
    printf "Part 2: %d\n" $ part2 l

-- Part 1: 57346
-- Part 2: 57345
