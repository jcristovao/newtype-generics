{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Control.NewtypeSpec where

import Prelude

import Data.Monoid
import Control.Newtype
import GHC.Generics

import Test.Hspec

newtype TestNewType = TestNewType Int deriving (Eq,Show,Generic)

instance Newtype TestNewType

{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = describe "Newtype test" $ do
  let four = 4 :: Int
      five = 5 :: Int
      noth = Nothing  :: Maybe String
  it "pack" $ do
    (pack True :: All)              `shouldBe` All True
    (pack True :: Any)              `shouldBe` Any True
    (pack (Just five) :: First Int) `shouldBe` First (Just 5)

  it "unpack" $ do
    unpack (Any False)          `shouldBe` False
    unpack (First (Just five))  `shouldBe` Just five
    unpack (Last noth)          `shouldBe` Nothing
    unpack (TestNewType five)   `shouldBe` five

  it "op" $ do
    op All (All True)  `shouldBe` True
    op Any (Any False) `shouldBe` False
    op Sum (Sum five)  `shouldBe` five

  it "under" $ do
    let sumLess (Sum x) = Sum (x - 1)
        firstN  (_)     = First Nothing
    under Sum   sumLess five        `shouldBe` four
    under First firstN  (Just five) `shouldBe` (Nothing :: Maybe Int)

  it "over" $ do
    over Sum     (+1) (Sum     four) `shouldBe` Sum five
    over Product (+1) (Product four) `shouldBe` Product five

  it "under2" $ do
    under2 Sum (<>) four five `shouldBe` 9

  it "over2" $ do
    over2 TestNewType (+) (TestNewType four) (TestNewType five) `shouldBe` TestNewType 9

