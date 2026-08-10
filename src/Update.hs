module Update where
import Collision
import Types
import Board
import Piece
import Game


movePieceDown :: GameBoard -> GameBoard
movePieceDown origBoard
    | getGameStatus origBoard == GameOver = origBoard
    | canMoveDown curPiece origBoard =
        origBoard {getCurrentPiece =
            curPiece{getPosition =
                 curPos{getHeightPos = curHeight +1 }}}
    | otherwise = fixCurPiece origBoard
    where
        curPiece = getCurrentPiece origBoard
        curPos = getPosition curPiece
        curHeight = getHeightPos curPos

fixCurPiece :: GameBoard -> GameBoard
fixCurPiece origBoard = 
    let clearedGameBoard = clearFullLines (fixPieceInBoard origBoard)
    in case checkGameOver clearedGameBoard of
        True  -> clearedGameBoard { getGameStatus = GameOver }
        False -> spawnNewPiece clearedGameBoard

fixPieceInBoard :: GameBoard -> GameBoard
fixPieceInBoard origBoard = insertPieceInBoard origBoard (fixPiece $ getCurrentPiece origBoard)

spawnNewPiece :: GameBoard -> GameBoard
spawnNewPiece origBoard = origBoard
    { getCurrentPiece = origPiece
        { getKind     = randomKind
        , getRotation = Deg0
        , getPosition = Position { getHeightPos = spawnRow, getWidthPos = spawnColumn }
        , getGrid     = usedSpace randomKind
        }
    , getRandomGen = newGen
    }
    where
        origPiece = getCurrentPiece origBoard
        (randomKind, newGen) = randomPieceKindPure (getRandomGen origBoard)
        spawnRow = getBoardTotalHeight origBoard - getBoardHeight origBoard - 1
        spawnColumn = (getBoardWidth origBoard - pieceSize randomKind) `div` 2

checkGameOver :: GameBoard -> Bool
checkGameOver origBoard = any (\y -> any (== Fixed) (getMatrix (getBoardGrid origBoard) !! y)) [bufferSize - 1, bufferSize - 2 .. 0]
    where
        bufferSize = getBoardTotalHeight origBoard - getBoardHeight origBoard
    
movePieceRight :: GameBoard -> GameBoard
movePieceRight origBoard
    | getGameStatus origBoard == GameOver = origBoard
    | canMoveRight curPiece origBoard =
        origBoard { getCurrentPiece = updatedPiece }
    | otherwise = origBoard
    where
        curPiece = getCurrentPiece origBoard
        curPos = getPosition curPiece
        updatedPiece = curPiece { getPosition = curPos { getWidthPos = getWidthPos curPos + 1 } }

movePieceLeft :: GameBoard -> GameBoard
movePieceLeft origBoard
    | getGameStatus origBoard == GameOver = origBoard
    | canMoveLeft curPiece origBoard =
        origBoard { getCurrentPiece = updatedPiece }
    | otherwise = origBoard
    where
        curPiece = getCurrentPiece origBoard
        curPos = getPosition curPiece
        updatedPiece = curPiece { getPosition = curPos { getWidthPos = getWidthPos curPos - 1} }

rotatePieceInBoard :: GameBoard -> GameBoard
rotatePieceInBoard origBoard
    | getGameStatus origBoard == GameOver = origBoard
    | canRotate = origBoard { getCurrentPiece = rotated }
    | otherwise = origBoard
    where
        curPiece = getCurrentPiece origBoard
        rotated = rotatePieceR curPiece
        canRotate = checkIfPieceLegal origBoard rotated

clearFullLines :: GameBoard -> GameBoard
clearFullLines origBoard = origBoard { getBoardGrid = Grid newRows }
  where
    rows = getMatrix (getBoardGrid origBoard)
    remainingRows = filter (not . all (== Fixed)) rows
    clearedCount = length rows - length remainingRows
    emptyRow = replicate (getBoardWidth origBoard) Empty
    newRows = replicate clearedCount emptyRow ++ remainingRows
