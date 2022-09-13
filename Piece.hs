module Piece(
    PieceType(..), Color(..), Piece(..), otherColor, pieceHasColor, updatePieceTurn, directionOfPawn
) where

data PieceType = Pawn | Bishop | Knight | King | Queen | Rook deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Piece = Piece Color PieceType (Maybe Int) deriving (Show, Eq)

otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

pieceHasColor :: Piece -> Color -> Bool
pieceHasColor (Piece c1 _ _) c2 = c1 == c2

updatePieceTurn :: Piece -> Int -> Piece
updatePieceTurn (Piece a b _) t = Piece a b (Just t)

directionOfPawn :: Color -> Int
directionOfPawn Black = -1
directionOfPawn White = 1