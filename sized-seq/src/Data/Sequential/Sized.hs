{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ExplicitNamespaces, FlexibleContexts, GADTs                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude                 #-}
{-# LANGUAGE StandaloneDeriving, TypeInType, TypeOperators                 #-}
module Data.Sequential.Sized
       ( Sized, append, head, tail, empty, (<|)
       , SomeSized(..)
       , singleton, index, find, fromSeq, fromList, reverse
       , Ordinal(..), type (<), type (>)
       ) where
import           Data.Foldable            (Foldable)
import           Data.Monoid              (Monoid (..), (<>))
import           Data.Sequential          (Seq)
import qualified Data.Sequential          as Seq
import           Data.Singletons          (Sing, SomeSing (..), fromSing,
                                           toSing)
import           Data.Singletons.TypeLits (KnownNat, natVal, withKnownNat)
import           Data.Traversable         (Traversable)
import           GHC.TypeNats             (type (*), type (+), type (-),
                                           type (<=), Nat)
import           Prelude                  (Bool (..), Eq (..), Functor (..),
                                           Integral (..), Maybe (..), Ord (..),
                                           Show (..), fromIntegral, ($), (.))

newtype Sized (n :: Nat) a = Sized { runSized :: Seq.Seq a }
                           deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

infix 4 <
type n < m = n + 1 <= m
type n > m = m < n

append :: Sized n a -> Sized m a -> Sized (n + m) a
append (Sized a) (Sized b) = Sized (a <> b)

head :: (n > 0) => Sized n a -> a
head = Seq.head . runSized

tail :: (n > 0) => Sized n a -> Sized (n - 1) a
tail = Sized . Seq.tail . runSized

(<|) :: a -> Sized n a -> Sized (1 + n) a
(<|) a = Sized . Seq.cons a . runSized

empty :: Sized 0 a
empty = Sized Seq.empty

singleton :: a -> Sized 1 a
singleton = Sized . Seq.singleton

data Ordinal (n :: Nat) where
  OLt :: (k < n) => Sing k -> Ordinal n

data SomeSized a where
  SomeSized :: KnownNat n => Sized n a -> SomeSized a

deriving instance Show a => Show (SomeSized a)

index :: Sized n a -> Ordinal n -> a
index (Sized l) (OLt k) = withKnownNat k $ Seq.index l $ fromIntegral $ natVal k

find :: (a -> Bool) -> Sized n a -> Maybe a
find p = Seq.find p . runSized

unsafeWithSize :: pxy n -> Seq a -> Sized n a
unsafeWithSize _ = Sized

fromSeq :: Seq a -> SomeSized a
fromSeq orig =
  case toSing (fromIntegral $ Seq.length orig) of
    SomeSing sn -> withKnownNat sn  $ SomeSized (unsafeWithSize sn orig)

fromList :: [a] -> SomeSized a
fromList = fromSeq . Seq.fromList

reverse :: Sized n a -> Sized n a
reverse = Sized . Seq.reverse . runSized

sort :: Ord a => Sized n a -> Sized n a
sort = Sized . Seq.sort . runSized
