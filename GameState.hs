{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module GameState (
    GameState(..), getNewGameState, gameOver
) where

import Board
import Move
import Piece
import Data.Maybe

data GameState = GameState { currBoard :: Board, currPlayer :: Color }

getNewGameState :: GameState -> IO GameState
getNewGameState gameState = do
    move <- getMove

    let newGameState = applyMove gameState move
    if isNothing newGameState
        then do
            putStrLn "Invalid move, try again"
            getNewGameState gameState
        else do
            return $ fromJust newGameState

applyMove :: GameState -> Move -> Maybe GameState
applyMove (GameState board player) move
    | isNothing piece = Nothing
    | not $ pieceHasColor (fromJust piece) player = Nothing
    | isNothing pieceMover = Nothing
    | otherwise = Just $ GameState (fromJust newBoard) (otherColor player)
    where
        piece :: Maybe Piece
        piece = pieceAtSquare $ squareAt board $ fst move

        pieceMover :: Maybe PieceMover
        pieceMover = fmap getPieceMover piece

        newBoard :: Maybe Board
        newBoard = maybe Nothing (\mover -> mover board move) pieceMover

gameOver :: GameState -> Bool
gameOver board = False