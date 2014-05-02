module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import DicewordsTest
import TrieTest

import Data.List

main = defaultMain tests

tests = [
	testGroup "Dicewords" [
		testProperty "diceString length" prop_diceString_length,
		testProperty "diceString interval" prop_diceString_interval,
		testProperty "diceString value" prop_diceString_value,
		testProperty "frequencies" prop_frequencies
	],
  testGroup "Trie" [
    testProperty "fromList preserve" prop_fromList_preserve,
    testProperty "prefixes" prop_prefixes,
    testProperty "delete" prop_delete,
    testProperty "member allElementsAreMember" prop_member_allElementsAreMember,
    testProperty "member nonElementsAreNotMember" prop_member_nonElementsAreNotMember
  ]]
