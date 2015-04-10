module Dicewords where

import qualified Trie
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (isAscii, isLetter, toLower, intToDigit)
import Data.List (unfoldr, sortBy, maximumBy, sort, foldl')
import Data.Ord (comparing)
import Prelude hiding (words)

-- The output mode. DicewareMode outputs words that can be formed with 5 dice
-- rolls, together with the corresponding dice rolls.
-- Diceware8k outputs 8k words, without the dice rolls.
data Mode = DicewareMode | Diceware8kMode | AllWordsMode deriving (Show, Eq)

-- Collected statistics
data Statistics = Statistics { averageWordLength::Float,
                               originalWordCount::Int,
                               filteredWordCount :: Int,
                               wordCount::Int } deriving Show

-- A word
type Word = String

maximumWordSize :: Int
maximumWordSize = 6 

-- Return a map with the number of occurrences of each element in the list
frequencies :: Ord a => [a] -> Map.Map a Int
frequencies = foldl' (\m word -> Map.insertWith (+) word 1 m) Map.empty

-- For a word, returns all the possible decompositions of the word.
-- E.g., for "foobar", and list of ["foo", "bar", "foob", "fooba", "ar"]
-- returns [["foo", "bar"], ["foob", "ar"]]
decompose :: Word -> Trie.Trie Char -> [[String]]
decompose word words = concatMap decomposeSuffix $ Trie.prefixes word words
  where
    decomposeSuffix prefix 
      | length prefix == length word = [[prefix]]
      | otherwise = map (prefix:) (decompose (drop (length prefix) word) words)

-- Removes all composite words (words that are a combination of other words),
-- after filtering out words that frequently occur in other words (to avoid
-- excluding too many words).
-- This is the most heavy part of the algorithm. It's not yet fully optimized,
-- and the solution is computed very greedily, so may not be optimal.
filterCompositeWords :: [Word] -> [Word]
filterCompositeWords = filterCompositeWords' . Trie.fromList

filterCompositeWords' :: Trie.Trie Char -> [Word]
filterCompositeWords' words
  | maxFrequency < 2 = nonComposedWords
  | otherwise = filterCompositeWords' $ Trie.delete maxComponent words
  where 
    decomposedWords = map (`decompose` words) (Trie.toList words)
    componentFrequencies = Map.toList $ frequencies $ concat $ concat decomposedWords
    (maxComponent, maxFrequency) = maximumBy (comparing snd) componentFrequencies
    nonComposedWords = map (head.head) $ filter (\x -> length x == 1) decomposedWords

-- Higher score is worse
score :: Word -> [Word] -> Int
score word _ = length word

sortByScore :: [Word] -> [Word]
sortByScore words = sortBy (comparing (`score` words)) words

{-# ANN filterWords "HLint: ignore Use map once" #-}
-- Runs the word list through a set of filters.
-- Returns a sorted list, with the preferred words first
filterWords :: [Word] -> [Word]
filterWords = 
  sortByScore .
  filterCompositeWords .
  filter (all (\c -> isAscii c && isLetter c)) .
  filter (\l -> length l <= maximumWordSize) .
  Set.toList . Set.fromList .
  map (filter (/= '.')) .
  map (map toLower)

-- Decompose a number in the list of digits for a given base
digits :: Integral a => a -> a -> [a]
digits base number = reverse $ unfoldr modAndDiv number
  where
    modAndDiv 0 = Nothing
    modAndDiv i = Just (i `mod` base, i `div` base)

-- Creates a string of dice rolls for a given number
diceString :: Int -> Int -> String
diceString rollCount i = map (intToDigit . (+1)) paddedBase6
  where
    base6 = digits 6 i
    paddedBase6 = replicate (rollCount - length base6) 0 ++ base6

-- Trims a list of words to the right size, according to the mode
trim :: Mode -> [Word] -> [Word]
trim DicewareMode = take (6^(5::Integer))
trim Diceware8kMode = take (2^(13::Integer))
trim AllWordsMode = id

-- Takes a list of words, and prepares it for output.
-- Trims the number of words, and adds the dice rolls in DicewareMode.
markup :: Mode -> [Word] -> [Word]
markup DicewareMode words = zipWith (\x y -> x ++ " " ++ y) rolls $ sort words
  where
    rollCount = ceiling (logBase 6 (fromIntegral (length words)) :: Double)
    rolls = map (diceString rollCount) [0..]
markup Diceware8kMode words = sort words
markup AllWordsMode words = sort words

process :: String -> Mode -> (String, Statistics)
process input mode = (unlines resultWords, statistics)
  where
    originalWords = lines input
    filteredWords = filterWords originalWords
    trimmedWords = trim mode filteredWords
    resultWords = markup mode trimmedWords
    statistics = Statistics { 
      originalWordCount = length originalWords,
      filteredWordCount = length filteredWords,
      wordCount = length trimmedWords ,
      averageWordLength = fromIntegral (sum (map length trimmedWords)) / 
                          fromIntegral (length trimmedWords) }
