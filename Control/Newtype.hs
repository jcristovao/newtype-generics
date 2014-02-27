{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{- |
The 'Newtype' typeclass and related functions: 'op', 'ala', 'ala'', 'under'. Primarly pulled from Conor McBride's Epigram work. Some examples:

@ala Sum foldMap [1,2,3,4] -- foldMaps the list ala the Sum newtype. This results in 10.@

@ala Product foldMap [1,2,3,4] -- foldMaps the list ala the Product newtype. This results in 24.@

@ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3 -- foldMaps the list ala the Endo newtype. This results in 8.@

NB: 'Data.Foldable.foldMap' is a generalized @mconcatMap@ which is a generalized @concatMap@.

This package includes 'Newtype' instances for all the (non-GHC\/foreign) newtypes in base (as seen in the examples). However, there are neat things you can do with this with /any/ newtype and you should definitely define your own 'Newtype' instances for the power of this library. For example, see @ala Cont traverse@, with the proper 'Newtype' instance for Cont.
-}
module Control.Newtype ( Newtype(..), op, ala, ala', under, over, underF, overF ) where

import Data.Monoid
import Control.Applicative
import Control.Arrow
import GHC.Generics
{-import Generics.Deriving-}

-- | Given a newtype @n@, we will always have the same unwrapped type @o@, meaning we can represent this with a fundep @n -> o@.
--
-- Any instance of this class just needs to let @pack@ equal to the newtype's constructor, and let @unpack@ destruct the newtype with pattern matching.
{-class Newtype n o | n -> o where-}
  {-pack :: o -> n-}
  {-unpack :: n -> o-}


-- Generic Newtype
class GNewtype n where
  type GO n :: *
  gpack   :: GO n -> n p
  gunpack :: n p  -> GO n

-- We only need one instance, if these generic functions are only to work for
-- newtypes, as these have a fixed form. For example, for a newtype X = Y,
-- Rep X = D1 ... (C1 ... (S1 ... (K1 ... Y)))
instance GNewtype (D1 d (C1 c (S1 s (K1 i a)))) where
  type GO (D1 d (C1 c (S1 s (K1 i a)))) = a
  gpack   x                     = M1 (M1 (M1 (K1 x)))
  gunpack (M1 (M1 (M1 (K1 x)))) = x

-- Original Newtype class, extended with generic defaults (trivial) and deprived
-- of the second type argument (less trivial, as it involves a type family with
-- a default, plus an equality constraint for the related type family in
-- GNewtype). We do get rid of MultiParamTypeClasses and FunctionalDependencies,
-- though.

-- | As long as the type @n@ is an instance of Generic, you can create an instance
-- with just @instance Newtype n@
class (O n ~ GO (Rep n)) => Newtype n where
  type O n :: *
  type O n = GO (Rep n)

  pack   :: O n -> n
  default pack :: (Generic n, GNewtype (Rep n)) => O n -> n
  pack = to . gpack

  unpack :: n -> O n
  default unpack :: (Generic n, GNewtype (Rep n)) => n -> O n
  unpack = gunpack . from

{-
This would be nice, but it breaks in odd ways with GHC < 7.
Oh, and it also makes the instances an extra line longer. :(
class Newtype n where
  type Orig n
  pack :: Orig n -> n
  unpack :: n -> Orig n
-}

-- |
-- This function serves two purposes:
--
-- 1. Giving you the unpack of a newtype without you needing to remember the name.
--
-- 2. Showing that the first parameter is /completely ignored/ on the value level, meaning the only reason you pass in the constructor is to provide type information. Typeclasses sure are neat.
op :: (Newtype n, o ~ GO (Rep n)) => (o -> n) -> n -> o
op _ = unpack

-- | The workhorse of the package. Given a pack and a \"higher order function\", it handles the packing and unpacking, and just sends you back a regular old function, with the type varying based on the hof you passed.
--
-- The reason for the signature of the hof is due to 'ala' not caring about structure. To illustrate why this is important, another function in this package is 'under'. It is not extremely useful; @under2@ might be more useful (with e.g., @mappend@), but then we already digging the trench of \"What about @under3@? @under4@?\". The solution utilized here is to just hand off the \"packer\" to the hof. That way your structure can be imposed in the hof, whatever you may want it to be (e.g., List, Traversable).
ala :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n')) => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work. The way it differs from the 'ala' function in this package, is that it provides an extra hook into the \"packer\" passed to the hof. However, this normally ends up being @id@, so 'ala' wraps this function and passes @id@ as the final parameter by default. If you want the convenience of being able to hook right into the hof, you may use this function.
ala' :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n')) => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f = unpack . hof (pack . f)

-- | A very simple operation involving running the function \'under\' the newtype. Suffers from the problems mentioned in the 'ala' function's documentation.
under :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n')) => (o -> n) -> (n -> n') -> (o -> o')
under _ f = unpack . f . pack

-- | The opposite of 'under'. I.e., take a function which works on the underlying types, and switch it to a function that works on the newtypes.
over :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n')) => (o -> n) -> (o -> o') -> (n -> n')
over _ f = pack . f . unpack

-- | 'under' lifted into a Functor.
underF :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n'), Functor f) => (o -> n) -> (f n -> f n') -> (f o -> f o')
underF _ f = fmap unpack . f . fmap pack

-- | 'over' lifted into a Functor.
overF :: (Newtype n, o ~ GO (Rep n), Newtype n', o' ~ GO (Rep n'), Functor f) => (o -> n) -> (f o -> f o') -> (f n -> f n')
overF _ f = fmap pack . f . fmap unpack

-- why aren't those defined in base?
deriving instance Generic All
deriving instance Generic Any
deriving instance Generic (Sum a)
deriving instance Generic (Product a)
deriving instance Generic (Kleisli m a b)
deriving instance Generic (WrappedMonad m a)
deriving instance Generic (WrappedArrow a b c)
deriving instance Generic (ZipList a)
deriving instance Generic (Const a x)
deriving instance Generic (Endo a)
deriving instance Generic (First a)
deriving instance Generic (Last a)
deriving instance Generic (ArrowMonad a b)

-- original instances
instance Newtype All
instance Newtype Any
instance Newtype (Sum a)
instance Newtype (Product a)
instance Newtype (Kleisli m a b)
instance Newtype (WrappedMonad m a)
instance Newtype (WrappedArrow a b c)
instance Newtype (ZipList a)
instance Newtype (Const a x)
instance Newtype (Endo a)
instance Newtype (First a)
instance Newtype (Last a)
instance ArrowApply a => Newtype (ArrowMonad a b)
