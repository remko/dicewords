module DicewordsTest where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import qualified Data.Map as M
import Dicewords

prop_diceString_length :: Int -> Int -> Property
prop_diceString_length count i =
  count > 0 ==>
  i >= 0 && i < (6^count) ==>
  (length (diceString count i)) == count

prop_diceString_interval :: Int -> Int -> Property
prop_diceString_interval count i =
  count > 0 ==>
  i >= 0 && i < (6^count) ==>
  all (\x -> x >= '1' && x <= '6') (diceString count i)

prop_diceString_value :: Int -> Int -> Property
prop_diceString_value count i =
  count > 0 ==>
  i >= 0 && i < (6^count) ==>
  let result = diceString count i
      digits = map ((\x -> x-1) . (read::String->Int) . (\x -> [x])) result
      number = sum $ zipWith (*) (reverse digits) (iterate (*6) 1)
  in i == number

prop_frequencies :: [String] -> Bool
prop_frequencies s =
  sort s == sort expandedFrequencies
  where expandedFrequencies = concatMap (\(w,c) -> replicate c w) $ M.toList $ frequencies s
