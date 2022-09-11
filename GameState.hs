module GameState (
    GameState(..), getNewGameState, gameOver
) where

import Board
import Move
import Piece
import Data.Maybe
import Position

data GameState = GameState { currBoard :: Board, currPlayer :: Color }

getNewGameState :: GameState -> IO GameState
getNewGameState gameState = do
    move <- getMove

    let newBoard = applyMove (currBoard gameState) move
    if isNothing newBoard
        then do
            putStrLn "Invalid move, try again"
            getNewGameState gameState
        else do
            return $ GameState (fromJust newBoard) (otherColor $ currPlayer gameState)

gameOver :: GameState -> Bool
gameOver board = False