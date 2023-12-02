{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

import Data.List
import Data.Semigroup

data Draw = Draw {r, g, b :: Int} deriving (Show, Eq)
data Game a = Game Int a deriving (Show)

instance Ord Draw where
    compare x y
        | x == y = EQ
        | x.r <= y.r && x.g <= y.g && x.b <= y.b = LT
        | otherwise = GT

    x <= y = compare x y /= GT
    x < y = compare x y == LT
    x >= y = compare x y /= LT
    x > y = compare x y == GT

    max x y
        | x <= y = y
        | otherwise = x
    min x y
        | x <= y = x
        | otherwise = y

instance Semigroup Draw where
    Draw a b c <> Draw x y z = Draw (a + x) (b + y) (c + z)
instance Monoid Draw where
    mempty = Draw 0 0 0

type Input = [Game [Draw]]

space :: ReadP Char
space = char ' '

newline :: ReadP Char
newline = char '\n'

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseRed = do
    r <- parseInt
    void space
    void $ string "red"
    return $ mempty {r}

parseGreen = do
    g <- parseInt
    void space
    void $ string "green"
    return $ mempty {g}

parseBlue = do
    b <- parseInt
    void space
    void $ string "blue"
    return $ mempty {b}

parseSingleDraw = parseRed +++ parseGreen +++ parseBlue

parseDraw = sepBy parseSingleDraw (string ", ")

parseGame = do
    void $ string "Game "
    n <- parseInt
    void $ string ": "
    draws <- sepBy parseDraw (string "; ")
    return $ Game n (mconcat draws)

parseInput = sepBy parseGame newline

config :: Draw
config = Draw {r = 12, g = 13, b = 14}

combine :: Game [Draw] -> Game Draw
combine (Game n draws) =
    Game n $
        Draw
            { r = maximum $ map (.r) draws
            , g = maximum $ map (.g) draws
            , b = maximum $ map (.b) draws
            }

score :: Game Draw -> Maybe Int
score (Game n d@Draw {r, g, b})
    | d <= config = Just n
    | otherwise = Nothing

part1 :: Input -> Int
part1 = sum . mapMaybe (score . combine)

power :: Game Draw -> Int
power (Game n Draw {r, g, b}) = r * g * b

part2 :: Input -> Int
part2 = sum . map (power . combine)

main :: IO ()
main = do
    games <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 games
    printf "Part 2: %d\n" $ part2 games

-- Part 1: 2105
-- Part 2: 72422
