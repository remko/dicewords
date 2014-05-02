module Trie (
  Trie,
  fromList,
  toList,
  empty,
  insert,
  delete,
  member,
  prefixes,
) where

import qualified Data.Map as M

data Trie a = Trie { tIsTerminal :: Bool,
                     tChildren :: M.Map a (Trie a) }

instance (Show a) => Show (Trie a) where
  showsPrec d m  = showParen (d > 10) $ showString "fromList " . shows (toList m)

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldr insert empty

toList :: Trie a -> [[a]]
toList = toList' []
  where 
    toList' :: [a] -> Trie a -> [[a]]
    toList' acc trie =
      if tIsTerminal trie then (reverse acc):subtries
                             else subtries
      where
        subtries = concatMap (\(k,v) -> toList' (k:acc) v) 
                             (M.assocs (tChildren trie))

empty :: Trie a
empty = Trie { tIsTerminal = False, tChildren = M.empty }

insert :: (Ord a) => [a] -> Trie a -> Trie a
insert [] trie = trie { tIsTerminal = True }
insert (a:as) trie =
  trie { tChildren = M.insert a (insert as subtrie) children } 
  where 
    children = tChildren trie
    subtrie = case M.lookup a children of
                Nothing -> empty
                Just t -> t

prefixes :: (Ord a) => [a] -> Trie a -> [[a]]
prefixes x trie = prefixes' x trie []

prefixes' :: (Ord a) => [a] -> Trie a -> [a] -> [[a]]
prefixes' word trie acc 
  | tIsTerminal trie = (reverse acc):rest
  | otherwise = rest
  where rest = case word of 
                [] -> []
                (w:ws) -> case M.lookup w (tChildren trie) of
                            Nothing -> []
                            Just trie' -> prefixes' ws trie' (w:acc)

delete :: (Ord a) => [a] -> Trie a -> Trie a
delete [] trie = trie { tIsTerminal = False }
delete (a:as) trie = trie { tChildren = M.adjust (delete as) a (tChildren trie) }

member :: (Ord a) => [a] -> Trie a -> Bool
member [] trie = tIsTerminal trie
member (x:xs) trie = 
        case M.lookup x (tChildren trie) of
                Nothing -> False
                Just subtrie -> member xs subtrie
                                                                                        
