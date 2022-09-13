module Position(
    Position(..), Offset(..), validPosition, parsePosition, posAsPair, offsetFrom, addOffset, offsetRotations, offsetAsPair
) where

import Data.Char (chr, ord)

data Position = Position Int Int deriving (Show, Eq)
data Offset = Offset Int Int deriving (Show, Eq)

posAsPair :: Position -> (Int, Int)
posAsPair (Position x y) = (x, y)

offsetAsPair :: Offset -> (Int, Int)
offsetAsPair (Offset x y) = (x, y)

offsetFrom :: Position -> Position -> Offset
offsetFrom (Position x1 y1) (Position x2 y2) = Offset (x2 - x1) (y2 - y1)

addOffset :: Position -> Offset -> Position
addOffset (Position x y) (Offset dx dy) = Position (x + dx) (y + dy)

offsetRotations :: Offset -> [Offset]
offsetRotations (Offset 0 0) = [Offset 0 0]
offsetRotations (Offset dx dy) = [Offset dx dy, Offset dy (-dx), Offset (-dx) (-dy), Offset (-dy) dx]

validPosition :: Position -> Bool
validPosition (Position c r) = not (c < 0 || c >= 8 || r < 0 || r >= 8)

parsePosition :: Char -> Char -> Position
parsePosition cc rc = Position (ord cc - ord 'a') (ord rc - ord '1')