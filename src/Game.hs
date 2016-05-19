{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Game where

import Control.Lens
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.List

data Player = Player Int String | EmptyPlayer
  deriving (Eq,Show)

type Position = Int

data PositionData = PositionData Player Position
  deriving (Show, Eq)

type BoardData = [PositionData]

data FinishedBoard
data InPlayBoard

data Board a = Board [PositionData]
  deriving (Eq,Show)

maxBoardSize = 9

createBoard :: Board InPlayBoard
createBoard = Board []

playerAt :: Board a -> Position -> Maybe Player
playerAt board pos = case find (\(PositionData _ p)-> p == pos) b of
    Nothing                       -> Nothing
    Just (PositionData player _)  -> Just player
  where b = boardData board

positionIsOccupied :: Board a -> Position -> Bool
positionIsOccupied board pos = isJust $ playerAt board pos

boardData :: Board a -> BoardData
boardData (Board x) = x

move :: Board InPlayBoard -> Player -> Position -> Either String (Board InPlayBoard)
move board player pos = if (positionIsOccupied board pos) then errorMsg else updatePosAndReturnNewBoard
  where
    errorMsg = Left "Invalid position. Already occupied!!"
    updatePosAndReturnNewBoard = Right $ Board newPos
    newPos = ((boardData board) ++ [PositionData player pos])

isFinished :: Board InPlayBoard -> Maybe (Board FinishedBoard)
isFinished board = if noOfMoves >= 5 then Just $ Board bd else Nothing
  where
    noOfMoves = length bd
    bd = boardData board

whoWon :: Board FinishedBoard -> Player
whoWon board = fromJust $ playerAt board 0

takeBack :: Board a -> Board InPlayBoard
takeBack board = Board stripLastPos
  where
    stripLastPos = tail (boardData board)

playerA = Player 1 "PlayerA"
playerB = Player 2 "PlayerB"
