{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Either.Unwrap

import Game

main :: IO ()
main = do
  b <- return $ move createBoard playerA 1
  print $ b
  case b of
    Right br -> do
      print $ positionIsOccupied br 1
      print $ positionIsOccupied br 2
      nb <- return $ (fromRight $ move br playerB 3)
      print nb
      print $ takeBack nb
      br <- return $ fromRight $ move br playerA 2
      br <- return $ fromRight $ move br playerB 3
      br <- return $ fromRight $ move br playerA 5
      br <- return $ fromRight $ move br playerB 7
      br <- return $ fromRight $ move br playerA 6
      print $ br
      fb <- return $ isFinished br
      case fb of
        Nothing -> putStrLn "Game not over!!"
        Just _b -> print $ whoWon _b
    Left msg -> do
      putStrLn msg
  putStrLn "Done!!!"
