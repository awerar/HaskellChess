module Move(
    getMove, applyMove, Move
) where

import Position
import System.IO
import Data.Maybe
import Board
import Piece

type Move = (Position, Position)
applyMove :: Board -> Move -> Maybe Board
applyMove board (p1, p2) = do
    piece <- pieceAt board p1
    case pieceAt board p2 of
        Just _ -> Nothing
        Nothing -> do
            simplyMovePiece board p1 p2

type PieceMover = Board -> Position -> Position -> Maybe Board
moveRook :: PieceMover
moveRook = undefined

simplyMovePiece :: PieceMover
simplyMovePiece board p1 p2 = Just board2
        where
            board1 = replaceSquare board p1 Nothing
            board2 = replaceSquare board1 p2 $ pieceAt board p1

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