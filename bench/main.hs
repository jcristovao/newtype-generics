{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
module Main where

import Criterion
import Criterion.Main
import Control.Newtype.Generics
import Data.Coerce
import Data.Semigroup
import GHC.Generics

newtype MySumDerive = MySumDerive Int
  deriving (Generic)
instance Newtype MySumDerive
instance Semigroup MySumDerive where
  MySumDerive x <> MySumDerive y = MySumDerive (x + y)
instance Monoid MySumDerive where
  mappend = (<>)
  mempty = MySumDerive 0

newtype MySumManual = MySumManual Int
instance Newtype MySumManual where
  type O MySumManual = Int
  pack = MySumManual
  unpack (MySumManual x) = x
instance Semigroup MySumManual where
  MySumManual x <> MySumManual y = MySumManual (x + y)
instance Monoid MySumManual where
  mappend = (<>)
  mempty = MySumManual 0

mySumDerive :: [Int] -> Int
mySumDerive xs = ala MySumDerive foldMap xs

mySumManual :: [Int] -> Int
mySumManual xs = ala MySumManual foldMap xs

mySumOldschool :: [Int] -> Int
mySumOldschool xs = s
  where MySumDerive s = foldMap MySumDerive xs

mySumCoerce :: [Int] -> Int
mySumCoerce xs = coerce (foldMap coerce xs :: MySumDerive)

mySumCoerce' :: [Int] -> Int
mySumCoerce' xs = coerce (mconcat (coerce xs) :: MySumDerive)

preludeSum :: [Int] -> Int
preludeSum xs = sum xs

main :: IO ()
main = defaultMain [
    env (return [1..5 :: Int]) $ \ns ->
      let bench' s f = bench s (whnf f ns)
      in bgroup "[1..5 :: Int]"
        [ bgroup "foldMap"
            [ bench' "ala MySumDerive" mySumDerive
            , bench' "ala MySumManual" mySumManual
            , bench' "manual wrap & unwrap" mySumOldschool
            , bench' "coerce" mySumCoerce
            ]
        , bench' "coerce . mconcat . coerce" mySumCoerce'
        , bench' "Prelude.sum" preludeSum
        ]
  ]
