{-# LANGUAGE NoImplicitPrelude, Safe #-}
module Data.Sequential.List
   (Seq, Index,
    empty, singleton, fromList,

    -- * Basic interface
    cons, snoc, append, head, uncons, unsnoc,
    last, tail, init, null, length, compareLength,

    -- * Transforming sequences
    reverse, intersperse, intercalate, transpose, mapWithIndex,

    -- * Special folds
    concat, concatMap, any, all, maximum, minimum,

    -- * Unfolds and replicates
    replicate, concatReplicate, unfoldr,

    -- * Subsequences
    take, drop, splitAt, takeWhile,
    dropWhile, span, break,
    group, groupBy, inits, tails,
    splitOn, splitWhen, delete, nub,

    -- * Predicates
    isPrefixOf, isSuffixOf, isInfixOf,

    -- * View Patterns
    stripPrefix,

    -- * Searching
    elem, notElem, find, filter, partition,

    -- * Indexing sequences
    index, elemIndex, elemIndices,
    elemCount, findIndex, findIndices,

    -- * Zipping and unzipping
    zip, zipWith, unzip,

    -- * Ordered sequences
    sort,

    -- * Copying sequences
    copy
   ) where
import           Data.List
import           Data.List.Split (splitOn, splitWhen)
import           Prelude         (Eq (..), Maybe (..), Num (..), Ord (..),
                                  Ordering, (.))
import qualified Prelude         as P

type Seq a = [a]
type Index = P.Int

empty :: Seq a
empty = []

singleton :: a -> Seq a
singleton a = [a]

elemCount :: Eq a => a -> [a] -> Index
elemCount c = foldr (\a l -> if a == c then 1 + l else l) 0

append :: Seq a -> Seq a -> Seq a
append = (++)

snoc :: [a] -> a -> [a]
snoc xs a = xs ++ [a]

fromList :: [a] -> [a]
fromList = P.id

cons :: a -> Seq a -> Seq a
cons a xs = a : xs

unsnoc :: Seq a -> Maybe (Seq a, a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

copy :: Seq a -> Seq a
copy = P.id

index :: Seq a -> Index -> a
index = (P.!!)

concatReplicate :: Index -> Seq a -> Seq a
concatReplicate n = concat . P.replicate n

mapWithIndex :: (Index -> a -> b) -> Seq a -> Seq b
mapWithIndex f = zipWith f [0..]

compareLength :: Seq a -> Index -> Ordering
compareLength = compare . length
