{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import Control.Lens
import Control.Monad.Trans (liftIO)
import Data.Maybe

data Player = Player Int String | EmptyPlayer
  deriving (Eq,Show)

type Row = [Player]
type Board = [Row]

data Position = Position Int Int
  deriving (Eq,Show)


getPositionX :: Position -> Int
getPositionX (Position x _) = x

getPositionY :: Position -> Int
getPositionY (Position _ y) = y

boardSizeX = 3
boardSizeY = 3

createBoard :: Board
createBoard = take boardSizeY $ repeat . take boardSizeX $ repeat EmptyPlayer

getRow = element . getPositionX
getCol = element . getPositionY

playerAt :: Board -> Position -> Maybe Player
playerAt board pos = board ^? (element (getPositionX pos) . element (getPositionY pos))

positionIsOccupied :: Board -> Position -> Bool
positionIsOccupied board pos = hasPlayer $ playerAt board pos
  where
    hasPlayer (Just EmptyPlayer) = False
    hasPlayer _                  = True

move :: Board -> Player -> Position -> Board
move board player pos = (getRow pos . getCol pos .~ player) board

whoWon :: Board -> Player
whoWon board = fromJust $ playerAt board (Position 0 0)

takeBack :: Board -> Board
takeBack b = b

main :: IO ()
main = do
  b <- return $ move (createBoard) (Player 1 "PlayerA") (Position 1 1)
  print $ b
  isOccupied <- return $ positionIsOccupied b (Position 1 1)
  print isOccupied
  print $ positionIsOccupied b (Position 1 2)
  putStrLn "Game Over!!!"
