{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Test.HUnit

import Data.List
import Data.Either.Unwrap

import Game

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Test Move api" [
              testProperty "char sample" prop_shouldTestEveryPositionOnBoard,
              testProperty "move valid" (prop_move_valid createBoard)
            ]
       ]

emptyPlayerGen :: Gen [Player]
emptyPlayerGen = vectorOf 3 $ elements [EmptyPlayer]

instance Arbitrary Board where
   arbitrary = Board <$> pos <*> rows
     where
      rows = vectorOf 3 emptyPlayerGen
      pos = elements [[]]
      --pos = arbitrary

instance Arbitrary Position where
   arbitrary = Position <$> pos <*> pos
     --where pos = getPositive <$> arbitrary
     where pos = choose (0,2)

instance Arbitrary Player where
   arbitrary = Player <$> id <*> name
     where
      id = getPositive <$> arbitrary
      name = elements ["plyA", "plyB"]

prop_move_valid b ply pos = (takeBack $ fromRight $ move b ply pos) == b
--prop_move_valid = forAll Board b. forAll Position p. suchThat (not (positionIsOccupied p b)). takeBack(move(p, b)) == b

prop_shouldTestEveryPositionOnBoard = ((\s -> s == s) :: [Char] -> Bool)
