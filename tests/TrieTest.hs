module TrieTest where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM)
import qualified Data.Set as Set
import Data.List (isPrefixOf, sort, all, nub)
import qualified Data.List as List (delete)
import Trie
import Debug.Trace

instance (Ord a, Arbitrary a) => Arbitrary (Trie a) where
  arbitrary = liftM fromList $ arbitrary

-- Defining a small alphabet to make the tests to the point.
-- This is probably not how it's supposed to be done
data MyChar = A | B | C | D  deriving (Eq, Ord, Show)

instance Arbitrary MyChar where
  arbitrary = elements [A, B, C, D]


prop_fromList_preserve :: [[MyChar]] -> Bool
prop_fromList_preserve x = (sort (toList $ fromList x)) == (sort (nub x))

prop_prefixes :: Trie MyChar -> [MyChar] -> Bool
prop_prefixes trie w = 
  sort (prefixes w trie) == allPrefixes
  where allPrefixes = nub $ filter (flip isPrefixOf w) (toList trie)

prop_delete :: Trie MyChar -> Int -> Property
prop_delete trie i = 
  i>= 0 && i < length (toList trie) ==>
  toList (delete element trie) == List.delete element (toList trie)
  where
    element = (toList trie) !! i

prop_member_allElementsAreMember :: Trie MyChar -> Bool
prop_member_allElementsAreMember trie =
  all (flip member trie) $ toList trie

prop_member_nonElementsAreNotMember :: Trie MyChar -> [MyChar] -> Property
prop_member_nonElementsAreNotMember trie element =
  not (elem element (toList trie)) ==>
  not $ member element trie
