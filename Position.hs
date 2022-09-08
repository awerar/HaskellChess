module Position(
    Position, validPosition, parsePosition
) where

import Data.Char (chr, ord)

type Position = (Int, Int)

validPosition :: Position -> Bool
validPosition (c, r) = not (c < 0 || c >= 8 || r < 0 || r >= 8)

parsePosition :: Char -> Char -> Position
parsePosition cc rc = (ord cc - ord 'a', ord rc - ord '1')