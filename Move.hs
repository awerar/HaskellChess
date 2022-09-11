module Move(
    getMove, applyMove, Move
) where

import Position
import System.IO
import Data.Maybe
import Board
import Piece
import GameState

type Move = (Position, Position)
type PieceMover = Move -> Board -> Board
type MoveValidator = Move -> Board -> Bool

applyMove :: GameState -> Move -> Maybe GameState
applyMove (GameState board currPlayer) (p1, p2) = do
    piece <- pieceAt board p1

    let color = case piece of (Piece c _) -> c
    if color /= currPlayer then Nothing
    else do
        newBoard <- do
            let piece2 = pieceAt board p2
            mover <- case (piece, piece2) of
                    (Piece _ pt, Nothing) -> let
                        validator = case pt of
                            Rook -> moveValidForRook
                            _ -> (\_ _ -> False)
                        in (if validator (p1, p2) board then return movePiece else Nothing)
                    (_, Just _) -> Nothing

            return $ mover (p1, p2) board

        return $ GameState newBoard (otherColor currPlayer)

        

moveValidForRook :: MoveValidator
moveValidForRook board move = any (\delta -> destinationIsOnLine delta board move) (offsetRotations (Offset 1 0))

destinationIsOnLine :: Offset -> MoveValidator
destinationIsOnLine delta (p1, p2) board
    | not $ validPosition p1 = False
    | p1 == p2 = True
    | isJust (pieceAt board p1) = False
    | otherwise = destinationIsOnLine delta (addOffset p1 delta, p2) board

movePiece :: PieceMover
movePiece (p1, p2) board
    | isNothing (pieceAt board p1) = error "Trying to move an empty piece"
    | isJust (pieceAt board p2) = error "Trying to move an occupied square"
    | otherwise = let
        removedBoard = replaceSquare board p1 Nothing
        piece = pieceAt board p1
        in replaceSquare removedBoard p2 piece

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