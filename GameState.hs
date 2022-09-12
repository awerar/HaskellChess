module GameState (
    GameState(..), gameOver
) where

import Board
import Piece
import Data.Maybe
import Position

data GameState = GameState { currBoard :: Board, currPlayer :: Color, currTurn :: Int }

gameOver :: GameState -> Bool
gameOver board = False