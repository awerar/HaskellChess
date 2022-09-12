module Piece(
    PieceType(..), Color(..), Piece(..), otherColor, pieceHasColor
) where

data PieceType = Pawn | Bishop | Knight | King | Queen | Rook deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Piece = Piece Color PieceType (Maybe Int) deriving (Show, Eq)

otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

pieceHasColor :: Piece -> Color -> Bool
pieceHasColor (Piece c1 _ _) c2 = c1 == c2

updatePieceTurn :: Piece -> Turn -> Piece
updatePieceTurn (Piece a b _) t = Piece a b t