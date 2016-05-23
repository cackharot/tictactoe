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
playerAt (Board xs) pos = getPlayer $ find (\(PositionData _ p)-> p == pos) xs

getPlayer Nothing = Nothing
getPlayer (Just (PositionData player _)) = Just player

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
isFinished (Board xs) = if noOfMoves >= 5 && isJust (whoWon finishedBoard) then Just finishedBoard else Nothing
  where
    noOfMoves = length xs
    finishedBoard = Board xs

whoWon :: Board FinishedBoard -> Maybe Player
whoWon board
  | isPlayerWon playerA = Just playerA
  | isPlayerWon playerB = Just playerB
  | otherwise = Nothing
  where
    isPlayerWon p = any (==True) (winPos p)
    winPos p = fmap (\x->checkAtPos board x p) winPositions

winPositions = [[0,1,2], [3,4,5], [6,7,8]
               ,[0,3,6], [1,5,7], [2,6,8]
               ,[0,5,8], [2,5,6]]

checkAtPos :: Board FinishedBoard -> [Int] -> Player -> Bool
checkAtPos (Board xs) winPos player = length playerCells == 3
  where
    playerCells = filter (\(PositionData ply pos)-> ply == player && (any (== pos) winPos)) xs

takeBack :: Board a -> Board InPlayBoard
takeBack board = Board stripLastPos
  where
    stripLastPos = tail (boardData board)

playerA = Player 1 "PlayerA"
playerB = Player 2 "PlayerB"
