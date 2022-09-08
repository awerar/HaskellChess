module Board (
    Board, Square(..), startBoard, squareAt, replaceSquare, pieceAtSquare
) where

import Piece
import Position

data Square = Occupied Piece | Empty deriving Show
type Board = [[Square]]

instance Eq Square where
    Empty == Empty = True
    Occupied p1 == Occupied p2 = p1 == p2
    s1 == s2 = False

startBoardRC :: Board
startBoardRC = reverse $
    [
        map (Occupied . Piece Black) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook],
        replicate 8 $ Occupied $ Piece Black Pawn
    ] ++
    replicate 4 (replicate 8 Empty) ++
    [
        replicate 8 $ Occupied $ Piece White Pawn,
        map (Occupied . Piece White) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
    ]

startBoardCR :: Board
startBoardCR = [[startBoardRC !! r !! c | r <- [0..7]] | c <- [0..7]]

startBoard :: Board
startBoard = startBoardCR

squareAt :: Board -> Position -> Square
squareAt board (c, r) = (board !! c) !! r

pieceAtSquare :: Square -> Maybe Piece
pieceAtSquare Empty = Nothing
pieceAtSquare (Occupied piece) = Just piece

replaceSquare :: Board -> Position -> Square -> Board
replaceSquare board (c, r) square = replaceElement board c (replaceElement (board !! c) r square)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement [] _ _ = error "Index not in list"
replaceElement (n:ns) 0 new = new:ns
replaceElement (n:ns) i new = n:replaceElement ns (i-1) new