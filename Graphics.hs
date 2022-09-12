module Graphics(
    displayBoard, displayGameState
) where

import Board
import Piece
import Data.Char (chr, ord)
import Position
import GameState

displayGameState :: GameState -> String
displayGameState (GameState board Black _) = displayBoard board
displayGameState (GameState board White _) = unlines $ reverse $ lines $ displayBoard board

displayBoard :: Board -> String
displayBoard board = unlines $ [coordinateRow] ++ displayRows 0 ++ [coordinateRow]
    where 
        coordinateRow = "  a b c d e f g h"

        displayRows :: Int -> [String]
        displayRows 8 = []
        displayRows r = (show (r+1) ++ displayRow 0 r ++ " " ++ show (r+1)) : displayRows (r + 1)

        displayRow :: Int -> Int -> String
        displayRow 7 r = [displaySquare $ pieceAt board (Position 7 r)]
        displayRow c r = displaySquare (pieceAt board (Position c r)) : ' ' : displayRow (c+1) r

displaySquare :: Maybe Piece -> Char
displaySquare Nothing = '.'
displaySquare (Just piece) = displayPiece piece

displayPiece :: Piece -> Char
displayPiece (Piece c t _) = chr $ pieceTypeUnicode t + (if c == White then 0 else 6)

pieceTypeUnicode :: PieceType -> Int
pieceTypeUnicode t = case t of
    King -> 9812
    Queen -> 9813
    Rook -> 9814
    Bishop -> 9815
    Knight -> 9816
    Pawn -> 9817