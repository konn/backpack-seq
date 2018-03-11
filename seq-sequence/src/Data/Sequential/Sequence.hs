{-# LANGUAGE NoImplicitPrelude, PatternSynonyms, Safe #-}
module Data.Sequential.Sequence
   (
    Seq,
    Index,

    -- * Constructor and deconstructor
    empty, singleton, fromList,

    -- * Basic interface
    cons, snoc, append, head, uncons, unsnoc,
    last, tail, init, null, length, compareLength,

    -- * Transforming sequences
    reverse, intersperse, intercalate, mapWithIndex,

    -- * Special folds
    concat, concatMap, any, all, maximum, minimum,

    -- * Unfolds and replicates
    replicate, concatReplicate, unfoldr,

    -- * Subsequences
    take, takeEnd, drop, dropEnd, splitAt, takeWhile, takeWhileEnd,
    dropWhile, dropWhileEnd, span, spanEnd,
    break, breakEnd,
    -- breakOn, breakOnEnd,
    -- group, groupBy,
    inits, tails,
    -- splitOn, splitWhen,
    chunksOf, delete,

    -- * Predicates
    isPrefixOf, isSuffixOf,

    -- * View Patterns
    stripPrefix, stripSuffix,

    -- * Searching
    elem, notElem, find, filter, partition,

    -- * Indexing sequences
    index, elemIndex, elemIndices, elemIndexEnd,
    elemCount, findIndex, findIndices,

    -- * Zipping and unzipping
    zip, zipWith, unzip,

    -- * Ordered sequences
    sort,

    -- * Copying sequences
    copy
   )
   where
import           Control.Applicative hiding (empty)
import           Control.Monad       (join)
import qualified Data.Foldable       as F
import           Data.Maybe          (isJust)
import           Data.Monoid         (Monoid (..))
import           Data.Sequence
import           Prelude             (Bool, Eq (..), Functor (..), Int,
                                      Maybe (..), Num (..), Ord (..), Ordering,
                                      const, id, not, otherwise, (.))
import qualified Prelude             as P

type Index = Int

copy :: Seq a -> Seq a
copy = id

cons :: a -> Seq a -> Seq a
cons = (<|)

snoc :: Seq a -> a -> Seq a
snoc = (|>)

append :: Seq a -> Seq a -> Seq a
append = (><)

head :: Seq a -> a
head (a :<| _) = a

tail :: Seq a -> Seq a
tail (_ :<| as) = as

uncons :: Seq a -> Maybe (a, Seq a)
uncons Empty      = Nothing
uncons (a :<| as) = Just (a, as)

unsnoc :: Seq a -> Maybe (Seq a, a)
unsnoc Empty      = Nothing
unsnoc (as :|> a) = Just (as, a)

last :: Seq a -> a
last (_ :|> a) = a

init :: Seq a -> Seq a
init (as :|> _) = as

compareLength :: Seq a -> Index -> Ordering
compareLength = compare . length

concat :: [Seq a] -> Seq a
concat = mconcat

concatMap :: (a -> Seq b) -> Seq a -> Seq b
concatMap f = join . fmap f

intercalate :: Seq a -> Seq a -> Seq a
intercalate _ Empty = empty
intercalate _ (x :<| Empty) = singleton x
intercalate ys (x :<| xs) =
  x <| ys >< intercalate ys xs

findIndices :: (a -> Bool) -> Seq a -> [Index]
findIndices = findIndicesL

findIndex :: (a -> Bool) -> Seq a -> Maybe Index
findIndex = findIndexL

find :: (a -> Bool) -> Seq a -> Maybe a
find p Empty = Nothing
find p (a :<| as)
  | p a = Just a
  | otherwise = find p as

findIndexEnd :: (a -> Bool) -> Seq a -> Maybe Index
findIndexEnd = findIndexR

elemCount :: Eq a => a -> Seq a -> Index
elemCount c = length . filter (== c)

elemIndex, elemIndexEnd :: Eq a => a -> Seq a -> Maybe Index
elemIndex = elemIndexL
elemIndexEnd = elemIndexR

elemIndices :: Eq a => a -> Seq a -> [Index]
elemIndices = elemIndicesL

notElem, elem :: Eq a => a -> Seq a -> Bool
elem a = isJust . elemIndex a

notElem a = not . elem a

stripPrefix, stripSuffix :: Eq a => Seq a -> Seq a -> Maybe (Seq a)
stripPrefix Empty bs = Just bs
stripPrefix (a :<| as) (b :<| bs)
  | a == b = stripPrefix as bs
stripPrefix _ _ = Nothing

stripSuffix Empty bs = Just bs
stripSuffix (as :|> a) (bs :|> b)
  | a == b = stripSuffix as bs
stripSuffix _ _ = Nothing

isPrefixOf, isSuffixOf :: Eq a => Seq a -> Seq a -> Bool
isPrefixOf a = isJust . stripPrefix a
isSuffixOf a = isJust . stripSuffix a

delete :: Eq a => a -> Seq a -> Seq a
delete _ Empty = Empty
delete a (b :<| bs)
  | a == b = bs
  | otherwise = delete a bs

break, breakEnd, span, spanEnd :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
break = breakl
breakEnd = breakr
span = spanl
spanEnd = spanr

dropWhile, dropWhileEnd, takeWhile, takeWhileEnd :: (a -> Bool) -> Seq a -> Seq a
dropWhile = dropWhileL
dropWhileEnd = dropWhileR
takeWhile = takeWhileL
takeWhileEnd = takeWhileR

takeEnd :: Index -> Seq a -> Seq a
takeEnd n _          | n <= 0 = Empty
takeEnd _ Empty      = Empty
takeEnd n (xs :|> x) = takeEnd (n - 1) xs :|> x

dropEnd :: Index -> Seq a -> Seq a
dropEnd n xs         | n <= 0 = xs
dropEnd _ Empty      = Empty
dropEnd n (xs :|> x) = dropEnd (n - 1) xs

concatReplicate :: Index -> Seq a -> Seq a
concatReplicate n = join . replicate n

minimum, maximum :: Ord a => Seq a -> a
minimum = F.minimum
{-# INLINE minimum #-}

maximum = F.maximum
{-# INLINE maximum #-}

any, all :: (a -> Bool) -> Seq a -> Bool
any = F.any
{-# INLINE any #-}
all = F.all
{-# INLINE all #-}
