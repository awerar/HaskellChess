--chcp.com 65001
module Game where

import Board
import Graphics
import Position
import Control.Monad
import Move
import Piece
import Data.Maybe
import GameState

test = map (\x -> fst x) $ getAllValidMoves (GameState startBoard White 1)

main :: IO()
main = stepGame $ GameState startBoard White 1

stepGame :: GameState -> IO()
stepGame gameState = do
    putStrLn $ displayGameState gameState

    newGameState <- getNewGameState gameState

    unless (gameOver newGameState) $ do
        stepGame newGameState

getNewGameState :: GameState -> IO GameState
getNewGameState gameState = do
    move <- getMove

    let newGameState = applyMoveCheck gameState move
    if isNothing newGameState
        then do
            putStrLn "Invalid move, try again"
            getNewGameState gameState
        else return $ fromJust newGameState
