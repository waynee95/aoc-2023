import Control.Monad (void)
import Data.Char (isDigit)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

import qualified Data.IntMap as M
import qualified Data.Set as S

data Card = Card Int [Int] [Int]
    deriving (Show, Eq, Ord)

type Input = [Card]

spaces :: ReadP [Char]
spaces = many1 (char ' ')

newline :: ReadP Char
newline = char '\n'

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseCard :: ReadP Card
parseCard = do
    void $ string "Card"
    void $ spaces
    n <- parseInt
    void $ string ":"
    void $ spaces
    winning <- sepBy parseInt spaces
    void $ spaces
    void $ string "|"
    void $ spaces
    owning <- sepBy parseInt spaces
    return $ Card n winning owning

parseInput :: ReadP Input
parseInput = sepBy parseCard newline

part1 :: Input -> Int
part1 = sum . map points
  where
    points (Card _ winning owning)
        | S.null wins = 0
        | otherwise = 2 ^ (S.size wins - 1)
      where
        wins = S.intersection (S.fromList winning) (S.fromList owning)

part2 :: Input -> Int
part2 cards =
    let m = foldr play start cards
     in sum $ M.elems m
  where
    start = M.fromList [(n, 1) | (Card n _ _) <- cards]
    play (Card n winning owning) map = M.insert n copies map
      where
        wins = S.size $ S.intersection (S.fromList winning) (S.fromList owning)
        copies = sum [map M.! (n + i) | i <- [0 .. wins]]

main :: IO ()
main = do
    cards <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 cards
    printf "Part 2: %d\n" $ part2 cards

-- Part 1: 21485
-- Part 2: 11024379
