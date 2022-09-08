module Move(
    BoardMove, PieceMover, getMove, simplyMovePiece, getPieceMover, moveAsPair
) where

import Position
import System.IO
import Data.Maybe
import Board
import Piece

type PieceMover = Board -> BoardMove -> Maybe Board
type OffsetPieceMover = Board -> OffsetMove -> Maybe Board

newtype BoardMove = BoardMove (Position, Position)
newtype OffsetMove = OffsetMove (Position, Offset)

getPieceMover :: Piece -> PieceMover
getPieceMover (Piece _ Rook) = validateMover $ positionMoverFromOffsetMover moveRook
getPieceMover (Piece _ _) = validateMover simplyMovePiece

moveAsPair :: BoardMove -> (Position, Position)
moveAsPair (BoardMove p) = p

positionMoverFromOffsetMover :: OffsetPieceMover -> PieceMover
positionMoverFromOffsetMover mover board (BoardMove (p1, p2)) = mover board (OffsetMove (p1, offsetFrom p1 p2))

validateMover :: PieceMover -> PieceMover
validateMover mover board (BoardMove (p1, p2))
    | squareAt board p1 == Empty = Nothing
    | squareAt board p2 /= Empty = Nothing
    | otherwise = mover board $ BoardMove (p1, p2)

mergeMovers :: [Board -> m -> Maybe Board] -> (Board -> m -> Maybe Board)
mergeMovers [] board move = Nothing
mergeMovers (mover:movers) board move = if isNothing newBoard then mergeMovers movers board move else newBoard
    where
        newBoard :: Maybe Board
        newBoard = mover board move

moveRook :: OffsetPieceMover
moveRook = mergeMovers [
        moveDirection (Offset (0, 1)) 8,
        moveDirection (Offset (0, -1)) 8,
        moveDirection (Offset (1, 0)) 8,
        moveDirection (Offset (-1, 0)) 8
    ]

moveDirection :: Offset -> Int -> OffsetPieceMover
moveDirection (Offset (sx, sy)) dist board (OffsetMove (Position (x1, y1), Offset (dx, dy)))
    | sx == 0 && sy == 0 = error "Can't move in no direction"
    | not (validStep sx dx) = Nothing
    | not (validStep sy dy) = Nothing
    | sx /= 0 && sy /= 0 && (distX /= distY || max distX distY > dist) = Nothing
    | otherwise = simplyMovePieceOffset board (OffsetMove (Position (x1, y1), Offset (dx, dy)))
    where
        distX = if sx == 0 then 0 else dx `div` sx
        distY = if sy == 0 then 0 else dy `div` sy

        validStep :: Int -> Int -> Bool
        validStep sk dk = (sk == 0 && dk == 0) || (sk /= 0 && dk `mod` sk == 0)

simplyMovePiece :: PieceMover
simplyMovePiece board (BoardMove (p1, p2)) = Just board2
        where
            board1 = replaceSquare board p1 Empty
            board2 = replaceSquare board1 p2 $ squareAt board p1

simplyMovePieceOffset :: OffsetPieceMover
simplyMovePieceOffset board (OffsetMove (pos, delta)) = simplyMovePiece board (BoardMove (pos, addOffset pos delta))

getMove :: IO BoardMove
getMove = do
    putChar '?'
    hFlush stdout
    move <- readMove

    if isNothing move
        then do
            putStrLn "Badly formatted move, try again"
            getMove
        else do return (fromJust move)

readMove :: IO (Maybe BoardMove)
readMove = do
    line <- getLine
    if length line /= 4 then return Nothing
    else do
        let p1 = parsePosition (head line) (line !! 1)
        let p2 = parsePosition (line !! 2) (line !! 3)

        return $ if validPosition p1 && validPosition p2 then Just (BoardMove (p1, p2)) else Nothing