module GameState (
    GameState(..), gameOver
) where

import Board
import Piece
import Data.Maybe
import Position

data GameState = GameState { currBoard :: Board, currPlayer :: Color, currTurn :: Int }

gameOver :: GameState -> Bool
gameOver (GameState board player turn) = not $ any isKing (concat board)
    where
        isKing :: Maybe Piece -> Bool
        isKing Nothing = False
        isKing (Just (Piece c pt _)) = pt == King && c == player
