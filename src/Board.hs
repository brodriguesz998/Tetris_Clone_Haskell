module Board where
import Types

data GameBoard = GameBoard {getBoardHeight :: Int,
                            getBoardTotalHeight :: Int,
                            getBoardWidth :: Int,
                            getBoardGrid :: Grid}

--espera posições absolutas do tabuleiro
accessPosInBoard :: GameBoard -> Position -> CellState
accessPosInBoard origBoard origPos =
    if checkValid
        then accessPosInGrid (getBoardGrid origBoard) origPos
        else OutOfBounds
    where
        checkValid = (getHeightPos origPos <= getBoardHeight origBoard - 1) 
                  && (getHeightPos origPos >= 0) 
                  && (getWidthPos origPos <= getBoardWidth origBoard - 1) 
                  && (getWidthPos origPos >= 0)
foo :: Int
foo = soma1 10