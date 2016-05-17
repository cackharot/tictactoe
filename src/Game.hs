{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Game where

import Control.Lens
import Control.Monad.Trans (liftIO)
import Data.Maybe

data Player = Player Int String | EmptyPlayer
  deriving (Eq,Show)

type Row = [Player]
data Board = Board {
      boardMoves :: [Position]
    , boardData :: [Row]
  } deriving (Eq,Show)

data Position = Position Int Int
  deriving (Eq,Show)

getPositionX :: Position -> Int
getPositionX (Position x _) = x

getPositionY :: Position -> Int
getPositionY (Position _ y) = y

boardSizeX = 3
boardSizeY = 3

createBoard :: Board
createBoard = Board [] (take boardSizeY $ repeat . take boardSizeX $ repeat EmptyPlayer)

getRow = element . getPositionX
getCol = element . getPositionY

playerAt :: Board -> Position -> Maybe Player
playerAt board pos = (boardData board) ^? (element (getPositionX pos) . element (getPositionY pos))

positionIsOccupied :: Board -> Position -> Bool
positionIsOccupied board pos = hasPlayer $ playerAt board pos
  where
    hasPlayer (Just EmptyPlayer) = False
    hasPlayer _                  = True

updatePlayer board pos player = (getRow pos . getCol pos .~ player) (boardData board)

move :: Board -> Player -> Position -> Either String Board
move board player pos = if (positionIsOccupied board pos) then errorMsg else updatePosAndReturnNewBoard
  where
    errorMsg = Left "Invalid position. Already occupied!!"
    updatePosAndReturnNewBoard = Right $ Board ((boardMoves board) ++ [pos]) $ updatePlayer board pos player

whoWon :: Board -> Player
whoWon board = fromJust $ playerAt board (Position 0 0)

takeBack :: Board -> Board
takeBack board = Board stripLastPos $ updatePlayer board lastPos EmptyPlayer
  where
    posHistory = boardMoves board
    lastPos = last posHistory
    stripLastPos = tail posHistory

playerA = Player 1 "PlayerA"
playerB = Player 2 "PlayerB"
