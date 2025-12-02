module Aoc (LetterDirection (..), intoLetterDirection, readLines) where

data LetterDirection = L | R deriving Show

intoLetterDirection 'L' = Just L
intoLetterDirection 'R' = Just R
intoLetterDirection x = Nothing

readLines :: IO [String]
readLines = lines <$> getContents
