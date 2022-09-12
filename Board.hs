module Board (
    Board, startBoard, pieceAt, replaceSquare
) where

import Piece
import Position

type Board = [[Maybe Piece]]

startBoardRC :: Board
startBoardRC = reverse $
    [
        map (\x -> Just $ Piece Black x Nothing) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook],
        replicate 8 $ Just $ Piece Black Pawn Nothing
    ] ++
    replicate 4 (replicate 8 Nothing) ++
    [
        replicate 8 $ Just $ Piece White Pawn Nothing,
        map (\x -> Just $ Piece White x Nothing) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
    ]

startBoardCR :: Board
startBoardCR = [[startBoardRC !! r !! c | r <- [0..7]] | c <- [0..7]]

startBoard :: Board
startBoard = startBoardCR

pieceAt :: Board -> Position -> Maybe Piece
pieceAt board (Position c r) = (board !! c) !! r

replaceSquare :: Board -> Position -> Maybe Piece -> Board
replaceSquare board (Position c r) square = replaceElement board c (replaceElement (board !! c) r square)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement [] _ _ = error "Index not in list"
replaceElement (n:ns) 0 new = new:ns
replaceElement (n:ns) i new = n:replaceElement ns (i-1) new