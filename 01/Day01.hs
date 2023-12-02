import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

calibrationValue :: [Int] -> Int
calibrationValue xs = head xs * 10 + last xs

digitify :: String -> Maybe Int
digitify s
    | ("1", "one") `eitherIsPrefixOf` s = Just 1
    | ("2", "two") `eitherIsPrefixOf` s = Just 2
    | ("3", "three") `eitherIsPrefixOf` s = Just 3
    | ("4", "four") `eitherIsPrefixOf` s = Just 4
    | ("5", "five") `eitherIsPrefixOf` s = Just 5
    | ("6", "six") `eitherIsPrefixOf` s = Just 6
    | ("7", "seven") `eitherIsPrefixOf` s = Just 7
    | ("8", "eight") `eitherIsPrefixOf` s = Just 8
    | ("9", "nine") `eitherIsPrefixOf` s = Just 9
    | otherwise = Nothing
  where
    eitherIsPrefixOf (s1, s2) s = s1 `isPrefixOf` s || s2 `isPrefixOf` s

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
