{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Either.Unwrap

import Game

main :: IO ()
main = do
  b <- return $ move createBoard playerA (Position 0 2)
  print $ b
  case b of
    Right br -> do
      print $ positionIsOccupied br (Position 1 1)
      print $ positionIsOccupied br (Position 1 2)
      br <- return $ takeBack br
      print br
      nb <- return $ (fromRight $ move br playerB (Position 1 1))
      print nb
      print $ takeBack nb
    Left msg -> do
      putStrLn msg
  putStrLn "Done!!!"
