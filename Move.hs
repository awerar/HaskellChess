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
applyMove (GameState board currPlayer) (p1, p2) =
    if p1 == p2 then Nothing
    else do
        piece <- pieceAt board p1

        let color = case piece of (Piece c _) -> c
        if color /= currPlayer then Nothing
        else do
            newBoard <- do
                let piece2 = pieceAt board p2
                mover <- getMover piece piece2

                return $ mover (p1, p2) board

            return $ GameState newBoard (otherColor currPlayer)

    where
        getMover :: Piece -> Maybe Piece -> Maybe PieceMover
        getMover piece1 piece2 =
            case (piece1, piece2) of
                (Piece c pt, Nothing) -> getMoveMover c pt
                (_, Just _) -> Nothing

            where
                getMoveMover :: Color -> PieceType -> Maybe PieceMover
                getMoveMover c pt = if validator (p1, p2) board then return movePiece else Nothing
                    where
                        validator :: MoveValidator
                        validator = case pt of
                            Rook -> moveValidForRook
                            Bishop -> moveValidForBishop
                            Knight -> moveValidForKnight
                            Pawn -> moveValidForPawn c
                            _ -> (\_ _ -> False)

moveValidForRook :: MoveValidator
moveValidForRook move board = any (\delta -> destinationOnRay delta move board) (offsetRotations (Offset 1 0))

moveValidForBishop :: MoveValidator
moveValidForBishop move board = any (\delta -> destinationOnRay delta move board) (offsetRotations (Offset 1 1))

moveValidForKnight :: MoveValidator
moveValidForKnight move board = any (\delta -> destinationOnOffset delta move) (offsetRotations (Offset 1 2) ++ offsetRotations (Offset 2 1))

moveValidForPawn :: Color -> MoveValidator
moveValidForPawn owner move _ = destinationOnOffset offset move
    where
        offset = case owner of
            White -> Offset 0 1
            Black -> Offset 0 (-1)

destinationOnOffset :: Offset -> Move -> Bool
destinationOnOffset offset (p1, p2) = offset == (offsetFrom p1 p2)

destinationOnRay :: Offset -> MoveValidator
destinationOnRay delta move board = destinationIsOnLine delta (fst move) 8 move board

destinationIsOnLine :: Offset -> Position -> Int -> MoveValidator
destinationIsOnLine delta curr dist (p1, p2) board
    | not $ validPosition p1 = False
    | curr == p2 = True
    | dist == 0 = False
    | isJust (pieceAt board p1) && curr /= p1 = False
    | otherwise = destinationIsOnLine delta (addOffset curr delta) (dist - 1) (p1, p2) board

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
        else return (fromJust move)

readMove :: IO (Maybe Move)
readMove = do
    line <- getLine
    if length line /= 4 then return Nothing
    else do
        let p1 = parsePosition (head line) (line !! 1)
        let p2 = parsePosition (line !! 2) (line !! 3)

        return $ if validPosition p1 && validPosition p2 then Just (p1, p2) else Nothing