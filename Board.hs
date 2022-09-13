module Board (
    Board, startBoard, testBoard, pieceAt, replaceSquare
) where

import Piece
import Position
import Data.List

type Board = [[Maybe Piece]]

startBoard :: Board
startBoard = transpose $ reverse $
    [
        map (\x -> Just $ Piece Black x Nothing) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook],
        replicate 8 $ Just $ Piece Black Pawn Nothing
    ] ++
    replicate 4 (replicate 8 Nothing) ++
    [
        replicate 8 $ Just $ Piece White Pawn Nothing,
        map (\x -> Just $ Piece White x Nothing) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
    ]

testBoard = replaceSquare (replaceSquare startBoard (Position 1 0) Nothing) (Position 2 0) Nothing

pieceAt :: Board -> Position -> Maybe Piece
pieceAt board (Position c r) = (board !! c) !! r

replaceSquare :: Board -> Position -> Maybe Piece -> Board
replaceSquare board (Position c r) square = replaceElement board c (replaceElement (board !! c) r square)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement [] _ _ = error "Index not in list"
replaceElement (n:ns) 0 new = new:ns
replaceElement (n:ns) i new = n:replaceElement ns (i-1) new