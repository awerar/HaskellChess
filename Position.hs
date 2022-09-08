module Position(
    Position(..), Offset(..), validPosition, parsePosition, posAsPair, offsetFrom, addOffset
) where

import Data.Char (chr, ord)

newtype Position = Position (Int, Int)
newtype Offset = Offset (Int, Int)

posAsPair :: Position -> (Int, Int)
posAsPair (Position p) = p

offsetFrom :: Position -> Position -> Offset
offsetFrom (Position (x1, y1)) (Position (x2, y2)) = Offset (x2 - x1, y2 - y1)

addOffset :: Position -> Offset -> Position
addOffset (Position (x, y)) (Offset (dx, dy)) = Position (x + dx, y + dy)

validPosition :: Position -> Bool
validPosition (Position (c, r)) = not (c < 0 || c >= 8 || r < 0 || r >= 8)

parsePosition :: Char -> Char -> Position
parsePosition cc rc = Position (ord cc - ord 'a', ord rc - ord '1')