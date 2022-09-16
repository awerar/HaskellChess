module Move(
    getMove, applyMove, getAllValidMoves, Move
) where

import Position
import System.IO
import Data.Maybe
import Board
import Piece
import GameState

type Move = (Position, Position)
type PieceMover = Move -> Board -> Int -> Board
type MoveValidator = Move -> Board -> Int -> Bool

getAllValidMoves :: GameState -> [(Move, GameState)]
getAllValidMoves (GameState board player turn) = undefined
    where
        ownedPieces :: [(Piece, Position)]
        ownedPieces = [let p = Position x y in (fromJust (pieceAt board p), p) | 
            x <- [0..7], 
            y <- [0..7]
            , let r = do
                        piece <- pieceAt board $ Position x y
                        return $ pieceHasColor piece player
                    in isJust r && fromJust r]

getPiecePossibleOffsets :: Piece -> [Offset]
getPiecePossibleOffsets (Piece col Pawn _) = [
        Offset 0 dir,
        Offset 0 (2 * dir),
        Offset 1 dir,
        Offset (-1) dir
    ]
    where
        dir = directionOfPawn col

getPiecePossibleOffsets (Piece _ Rook _) = concat [offsetRotations $ Offset d 0 | d <- [1..7]]
getPiecePossibleOffsets (Piece _ Bishop _) = concat [offsetRotations $ Offset d d | d <- [1..7]]
getPiecePossibleOffsets (Piece c Queen t) = getPiecePossibleOffsets (Piece c Rook t) ++ (getPiecePossibleOffsets (Piece c Bishop t))
getPiecePossibleOffsets (Piece _ Knight _) = offsetRotations (Offset 1 2) ++ offsetRotations (Offset 2 1)
getPiecePossibleOffsets (Piece _ King _) = offsetRotations (Offset 1 0) ++ offsetRotations (Offset 0 1)

applyMove :: GameState -> Move -> Maybe GameState
applyMove (GameState board currPlayer turn) (p1, p2) =
    if p1 == p2 then Nothing
    else do
        piece <- pieceAt board p1

        if not (pieceHasColor piece currPlayer) then Nothing
        else do
            newBoard <- do
                let piece2 = pieceAt board p2
                mover <- getMover piece piece2

                return $ mover (p1, p2) board turn

            return $ GameState newBoard (otherColor currPlayer) (turn + 1)

    where
        getMover :: Piece -> Maybe Piece -> Maybe PieceMover
        getMover piece1 piece2 =
            case (piece1, piece2) of
                (Piece c pt lm, Nothing) -> getMoveMover c pt lm
                (Piece c1 pt1 lm1, Just (Piece c2 pt2 lm2)) ->
                    if c1 == c2 then
                        if pt1 == Rook && pt2 == King && isNothing lm1 && isNothing lm2 then Just moveCastle else Nothing
                    else getCaptureMover c1 pt1 lm1

            where
                getMoveMover :: Color -> PieceType -> Maybe Int -> Maybe PieceMover
                getMoveMover c pt lm = if validator (p1, p2) board turn then return movePiece else Nothing
                    where
                        validator :: MoveValidator
                        validator = case pt of
                            Rook -> moveValidForRook
                            Bishop -> moveValidForBishop
                            Knight -> moveValidForKnight
                            Pawn -> moveValidForPawn c lm
                            Queen -> moveValidForQueen
                            King -> moveValidForKing

                getCaptureMover :: Color -> PieceType -> Maybe Int -> Maybe PieceMover
                getCaptureMover c pt lm = if validator (p1, p2) board turn then return moveCapture else Nothing
                    where
                        validator :: MoveValidator
                        validator = case pt of
                            Rook -> moveValidForRook
                            Bishop -> moveValidForBishop
                            Knight -> moveValidForKnight
                            Pawn -> captureValidForPawn c
                            Queen -> moveValidForQueen
                            King -> moveValidForKing


moveValidForRook :: MoveValidator
moveValidForRook move board turn = any (\delta -> destinationOnRay delta move board) (offsetRotations (Offset 1 0))

moveValidForBishop :: MoveValidator
moveValidForBishop move board turn = any (\delta -> destinationOnRay delta move board) (offsetRotations (Offset 1 1))

moveValidForQueen :: MoveValidator
moveValidForQueen move board turn = moveValidForRook move board turn || moveValidForBishop move board turn

moveValidForKing :: MoveValidator
moveValidForKing move board turn = any (\delta -> destinationOnOffset delta move) (offsetRotations (Offset 0 1) ++ offsetRotations (Offset 1 1))

moveValidForKnight :: MoveValidator
moveValidForKnight move board turn = any (\delta -> destinationOnOffset delta move) (offsetRotations (Offset 1 2) ++ offsetRotations (Offset 2 1))

moveValidForPawn :: Color -> Maybe Int -> MoveValidator
moveValidForPawn owner lastMoved move board _ = destinationIsOnLine offset (fst move) dist move board
    where
        dist = if isNothing lastMoved then 2 else 1
        offset = Offset 0 $ directionOfPawn owner

captureValidForPawn :: Color -> MoveValidator
captureValidForPawn owner move board _ = destinationOnOffset (Offset 1 $ directionOfPawn owner) move ||
                                         destinationOnOffset (Offset (-1) $ directionOfPawn owner) move

destinationOnOffset :: Offset -> Move -> Bool
destinationOnOffset offset (p1, p2) = offset == offsetFrom p1 p2

destinationOnRay :: Offset -> Move -> Board -> Bool
destinationOnRay delta move = destinationIsOnLine delta (fst move) 8 move

destinationIsOnLine :: Offset -> Position -> Int -> Move -> Board -> Bool
destinationIsOnLine delta curr dist (p1, p2) board
    | not $ validPosition curr = False
    | curr == p2 = True
    | dist == 0 = False
    | isJust (pieceAt board curr) && curr /= p1 = False
    | otherwise = destinationIsOnLine delta (addOffset curr delta) (dist - 1) (p1, p2) board

moveCastle :: PieceMover
moveCastle (rp, kp) board turn = boardRookMoved
    where
        kingMoveDirection = signum $ fst $ offsetAsPair (offsetFrom kp rp)
        kingMovePosition = addOffset kp (Offset kingMoveDirection 0)
        boardKingMoved = movePiece (kp, kingMovePosition) board turn
        boardRookMoved = movePiece (rp, kp) boardKingMoved turn

moveCapture :: PieceMover
moveCapture (p1, p2) board = movePiece (p1, p2) (replaceSquare board p2 Nothing)

movePiece :: PieceMover
movePiece (p1, p2) board turn
    | isNothing (pieceAt board p1) = error "Trying to move an empty piece"
    | isJust (pieceAt board p2) = error "Trying to move an occupied square"
    | otherwise = let
        removedBoard = replaceSquare board p1 Nothing
        piece = updatePieceTurn (fromJust $ pieceAt board p1) turn
        in replaceSquare removedBoard p2 (Just piece)

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