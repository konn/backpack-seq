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
import qualified Data.List       as L
import           Data.List.Split (splitOn, splitWhen)
import           Prelude         (Bool, Eq (..), Maybe (..), Num (..), Ord (..),
                                  Ordering, (.))
import qualified Prelude         as P

type Seq = []
type Index = P.Int

empty :: Seq a
empty = []

singleton :: a -> Seq a
singleton a = [a]

elemCount :: Eq a => a -> Seq a -> Index
elemCount c = foldr (\a l -> if a == c then 1 + l else l) 0

append :: Seq a -> Seq a -> Seq a
append = (L.++)

snoc :: Seq a -> a -> Seq a
snoc xs a = xs L.++ [a]

fromList :: Seq a -> Seq a
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

length :: Seq a -> Index
length = P.length

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith = P.zipWith

zip :: Seq a -> Seq b -> Seq (a, b)
zip = P.zip

concat :: Seq (Seq a) -> Seq a
concat = L.concat

tail, init :: Seq a -> Seq a
init = L.init
tail = L.tail

head, last :: Seq a -> a
head = L.head
last = L.last

foldr :: (a -> b -> b) -> b -> Seq a -> b
foldr = L.foldr

foldl :: (a -> b -> a) -> a -> Seq b -> a
foldl = L.foldl

sort :: Ord a => Seq a -> Seq a
sort = L.sort

unzip :: Seq (a, b) -> (Seq a, Seq b)
unzip = L.unzip

findIndices :: (a -> P.Bool) -> Seq a -> [Index]
findIndices = L.findIndices

findIndex :: (a -> P.Bool) -> Seq a -> Maybe Index
findIndex = L.findIndex

elemIndices :: Eq a => a -> Seq a -> [Index]
elemIndices = L.elemIndices

elemIndex :: Eq a => a -> Seq a -> Maybe Index
elemIndex = L.elemIndex

partition, span, break :: (a -> P.Bool) -> Seq a -> (Seq a, Seq a)
partition = L.partition
span = L.span
break = L.break

filter :: (a -> P.Bool) -> Seq a -> Seq a
filter = L.filter

find :: (a -> P.Bool) -> Seq a -> Maybe a
find = L.find

elem, notElem :: Eq a => a -> Seq a -> P.Bool
notElem = L.notElem
elem = L.elem

stripPrefix :: Eq a => Seq a -> Seq a -> Maybe (Seq a)
stripPrefix = L.stripPrefix

isPrefixOf, isInfixOf, isSuffixOf :: Eq a => Seq a -> Seq a -> P.Bool
isPrefixOf = L.isPrefixOf
isSuffixOf = L.isSuffixOf
isInfixOf = L.isInfixOf

nub :: Eq a => Seq a -> Seq a
nub = L.nub

delete :: Eq a => a -> Seq a -> Seq a
delete = L.delete

inits, tails :: Seq a -> Seq (Seq a)
inits = L.inits
tails = L.tails

groupBy :: (a -> a -> P.Bool) -> Seq a -> Seq (Seq a)
groupBy = L.groupBy

group :: Eq a => Seq a -> Seq (Seq a)
group = L.group


dropWhile, takeWhile :: (a -> P.Bool) -> Seq a -> Seq a
dropWhile = L.dropWhile
takeWhile = L.takeWhile

splitAt :: Index -> Seq a -> (Seq a, Seq a)
splitAt = L.splitAt

drop, take :: Index -> Seq a -> Seq a
drop = L.drop
take = L.take

unfoldr :: (a -> Maybe (b, a)) -> a -> Seq b
unfoldr = L.unfoldr

replicate :: Index -> a -> Seq a
replicate = L.replicate

maximum, minimum :: Ord a => Seq a -> a
maximum = L.maximum
minimum = L.minimum

all, any :: (a -> P.Bool) -> Seq a -> P.Bool
all = L.all
any = L.any

concatMap :: (a -> Seq b) -> Seq a -> Seq b
concatMap = L.concatMap

transpose :: Seq (Seq a) -> Seq (Seq a)
transpose = L.transpose

uncons :: Seq a -> Maybe (a, Seq a)
uncons = L.uncons

null :: Seq a -> Bool
null = L.null

reverse :: Seq a -> Seq a
reverse = L.reverse

intersperse :: a -> Seq a -> Seq a
intersperse = L.intersperse

intercalate :: Seq a -> [Seq a] -> Seq a
intercalate = L.intercalate
