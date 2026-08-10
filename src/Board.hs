module Board where
import Types
import Piece
import Game
import System.Random(StdGen)

data GameBoard = GameBoard {getBoardHeight :: Int,
                            getBoardTotalHeight :: Int,
                            getBoardWidth :: Int,
                            getBoardGrid :: Grid,
                            getCurrentPiece :: Piece,
                            getGameStatus:: GameStatus,
                            getRandomGen :: StdGen}

newGameBoard :: StdGen -> GameBoard
newGameBoard gen = GameBoard
    { getBoardHeight = 20
    , getBoardTotalHeight = 24
    , getBoardWidth = 10
    , getBoardGrid = Grid (replicate 24 (replicate 10 Empty))
    , getCurrentPiece = Piece
        { getKind = I
        , getRotation = Deg0
        , getPosition = Position 3 3
        , getGrid = usedSpace I
        }
    , getGameStatus = Playing
    , getRandomGen = gen
    }

--espera posições absolutas do tabuleiro
accessPosInBoard :: GameBoard -> Position -> CellState
accessPosInBoard origBoard origPos =
    if checkValid
        then accessPosInGrid (getBoardGrid origBoard) origPos
        else OutOfBounds
    where
        checkValid = (getHeightPos origPos <= getBoardTotalHeight origBoard - 1)
                  && (getHeightPos origPos >= 0)
                  && (getWidthPos origPos <= getBoardWidth origBoard - 1)
                  && (getWidthPos origPos >= 0)

--performance terrível
changePosInBoard :: GameBoard -> Position -> CellState -> GameBoard
changePosInBoard origBoard origPos origState = 
    if checkValid
        then origBoard { getBoardGrid = newGrid }
        else origBoard
    where
        targetRow = getHeightPos origPos
        targetCol = getWidthPos origPos
        oldRows = getMatrix (getBoardGrid origBoard)
        newGrid = Grid (map updateRow (zip [0..] oldRows))
        updateRow (rowIdx, row) = 
            if rowIdx == targetRow
                then map updateCell (zip [0..] row)
                else row
        updateCell (colIdx, cell) = 
            if colIdx == targetCol
                then origState
                else cell
        checkValid = (getHeightPos origPos <= getBoardTotalHeight origBoard - 1)
                  && (getHeightPos origPos >= 0)
                  && (getWidthPos origPos <= getBoardWidth origBoard - 1)
                  && (getWidthPos origPos >= 0)

filledCellsWithOffsets :: Piece -> [(Int, Int)]
filledCellsWithOffsets origPiece = 
    [ (lineIdx, colIdx) 
    | (lineIdx, line) <- zip [0..] (getMatrix (getGrid origPiece))
    , (colIdx, cell) <- zip [0..] line
    , cell == Filled || cell == Fixed
    ]

insertPieceInBoard :: GameBoard -> Piece -> GameBoard
insertPieceInBoard origBoard origPiece = 
    foldr applyCell origBoard (filledCellsWithOffsets origPiece)
    where
        pieceHeight = getHeightPos (getPosition origPiece)
        pieceWidth  = getWidthPos (getPosition origPiece)
        applyCell (lineIdx, colIdx) board = 
            changePosInBoard board 
                (Position { getHeightPos = pieceHeight + lineIdx, getWidthPos = pieceWidth + colIdx }) 
                Fixed
