module Move(
    Move, PieceMover, getMove, simplyMovePiece, getPieceMover
) where

import Position
import System.IO
import Data.Maybe
import Board
import Piece

type PieceMover = Board -> Move -> Maybe Board
type Move = (Position, Position)

getPieceMover :: Piece -> PieceMover
getPieceMover (Piece _ _) = simplyMovePiece

simplyMovePiece :: PieceMover
simplyMovePiece board (p1, p2)
    | squareAt board p1 == Empty = Nothing
    | squareAt board p2 /= Empty = Nothing
    | otherwise = Just board2
        where
            board1 = replaceSquare board p1 Empty
            board2 = replaceSquare board1 p2 $ squareAt board p1 

getMove :: IO Move
getMove = do
    putChar '?'
    hFlush stdout
    move <- readMove

    if isNothing move
        then do
            putStrLn "Badly formatted move, try again"
            getMove
        else do return (fromJust move)

readMove :: IO (Maybe Move)
readMove = do
    line <- getLine
    if length line /= 4 then return Nothing
    else do
        let p1 = parsePosition (head line) (line !! 1)
        let p2 = parsePosition (line !! 2) (line !! 3)

        return $ if validPosition p1 && validPosition p2 then Just (p1, p2) else Nothing