{-# LANGUAGE NoImplicitPrelude, Trustworthy #-}
module Data.Sequential.Vector
   (
    Seq,
    Index,

    -- * Constructor and deconstructor
    empty, singleton, fromList,

    -- * Basic interface
    cons, cons', snoc, append, head, uncons, unsnoc,
    last, tail, init, null, length, compareLength,

    -- * Transforming sequences
    reverse, mapWithIndex,

    -- * Special folds
    concat, concatMap, any, all, maximum, minimum,

    -- * Unfolds and replicates
    replicate, concatReplicate, unfoldr, unfoldrN,

    -- * Subsequences
    take, drop, splitAt, takeWhile,
    dropWhile, span, spanEnd, break, breakEnd,

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
import           Control.Applicative          hiding (empty)
import           Control.Monad                (join)
import qualified Data.Foldable                as F
import           Data.Maybe                   (isJust)
import           Data.Monoid                  (Monoid (..))
import           Data.Vector
import qualified Data.Vector.Algorithms.Intro as VA
import           Prelude                      (Bool, Eq (..), Functor (..), Int,
                                               Integral (..), Maybe (..),
                                               Num (..), Ord (..), Ordering,
                                               const, id, not, otherwise, ($),
                                               (.))
import qualified Prelude                      as P

type Seq = Vector
type Index = Int

sort :: Ord a => Seq a -> Seq a
sort = modify VA.sort
{-# INLINE sort #-}

cons' :: a -> Seq a -> Seq a
cons' = cons
{-# INLINE cons' #-}

append :: Seq a -> Seq a -> Seq a
append = (++)
{-# INLINE append #-}

index :: Seq a -> Index -> a
index = (!)
{-# INLINE index #-}

elemCount :: Eq a => a -> Seq a -> Index
elemCount a = length . elemIndices a
{-# INLINE elemCount #-}

uncons :: Seq a -> Maybe (a, Seq a)
uncons xs =
  if null xs
  then Nothing
  else Just (head xs, tail xs)
{-# INLINE uncons #-}

unsnoc :: Seq a -> Maybe (Seq a, a)
unsnoc xs =
  if null xs
  then Nothing
  else Just (init xs, last xs)
{-# INLINE unsnoc #-}

mlast :: Seq a -> Maybe a
mlast xs =
  if null xs
  then Nothing
  else Just (last xs)
{-# INLINE mlast #-}

elemIndexEnd :: Eq a => a -> Seq a -> Maybe Index
elemIndexEnd x = mlast . elemIndices x
{-# INLINE elemIndexEnd #-}

findIndexEnd :: (a -> Bool) -> Seq a -> Maybe Index
findIndexEnd p = mlast . findIndices p
{-# INLINE findIndexEnd #-}

breakEnd, spanEnd :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakEnd p xs =
  case findIndexEnd p xs of
    Just i  -> (unsafeSlice 0 i xs, unsafeSlice i (length xs - i) xs)
    Nothing -> (xs, empty)
{-# INLINE breakEnd #-}

spanEnd p = breakEnd (not . p)
{-# INLINE spanEnd #-}

concatReplicate :: Int -> Seq a -> Seq a
concatReplicate c sq =
  let len = length sq
  in generate (c * len) $ \i -> sq ! (i `mod` len)

mapWithIndex :: (Index -> a -> b) -> Vector a -> Vector b
mapWithIndex = imap

compareLength :: Seq a -> Index -> Ordering
compareLength l = compare (length l)
