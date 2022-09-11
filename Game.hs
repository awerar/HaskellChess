--chcp.com 65001

import Board
import Graphics
import Position
import Control.Monad
import Move
import Piece
import Data.Maybe
import GameState

main :: IO()
main = stepGame $ GameState startBoard White

stepGame :: GameState -> IO()
stepGame gameState = do
    putStrLn $ displayGameState gameState

    newGameState <- getNewGameState gameState

    unless (gameOver newGameState) $ do
        stepGame newGameState

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
