module Board where
import Types

data GameBoard = GameBoard {getBoardHeight :: Int, getBoardWidth :: Int, getBoardGrid :: Grid}

accessPosInBoard :: GameBoard -> Position -> CellState
accessPosInBoard origBoard origPos = if (checkValid) then
    accessPosInGrid (getBoardGrid origBoard) origPos else
        OutOfBounds

    where
        checkValid = (getHeightPos origPos <= getBoardHeight origBoard) && (getHeightPos origPos >= 0) && (getWidthPos origPos <= getBoardWidth origBoard) && (getWidthPos origPos>= 0) 

foo :: Int
foo = soma1 10