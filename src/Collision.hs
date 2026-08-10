module Collision (module Collision) where
import Piece 
import Board
import Types
import Data.List (find)
import Data.Maybe (fromMaybe)

--aqui eu vou lidar com tudo referente a mudança de posição da peça no tabuleiro, verificar se a peça encostou no chao ou se o jogador perdeu
--accessPosInBoard espera coordenadas absolutas do tabuleiro e filledLinesOfaColumn e rightmostFilledColumn retornam coordenadas relativas da peça

canMoveDown :: Piece -> GameBoard -> Bool
canMoveDown origPiece origBoard = 
    not (checkFloorCollision origPiece origBoard) && not (any (\cell -> cell == Filled || cell == Fixed) cellsBelow)
    where
        origPos = getPosition origPiece
        targetLine = bottomMostFilledLine origPiece
        filledColumns = filledColumnsOfALine origPiece targetLine
        cellsBelow = map (\col -> accessPosInBoard origBoard 
                                    (Position { getHeightPos = getHeightPos origPos + targetLine + 1
                                              , getWidthPos = getWidthPos origPos + col
                                              })) 
                               filledColumns

canMoveRight :: Piece -> GameBoard -> Bool
canMoveRight origPiece origBoard = 
    not (checkWallCollisionRight origPiece origBoard) && not (any (\cell -> cell == Filled || cell == Fixed) cellsToTheRight)
    where
        origPos = getPosition origPiece
        targetColumn = rightmostFilledColumn origPiece
        filledLines = filledLinesOfaColumn origPiece targetColumn
        cellsToTheRight = map (\line -> accessPosInBoard origBoard 
                                    (Position { getHeightPos = getHeightPos origPos + line
                                              , getWidthPos = getWidthPos origPos + targetColumn + 1 
                                              })) 
                               filledLines

canMoveLeft :: Piece -> GameBoard -> Bool
canMoveLeft origPiece origBoard = 
    not (checkWallCollisionLeft origPiece) && not (any (\cell -> cell == Filled || cell == Fixed) cellsToTheLeft)
    where
        origPos = getPosition origPiece
        targetColumn = leftMostFilledColumn origPiece
        filledLines = filledLinesOfaColumn origPiece targetColumn
        cellsToTheLeft = map (\line -> accessPosInBoard origBoard 
                                    (Position { getHeightPos = getHeightPos origPos + line
                                              , getWidthPos = getWidthPos origPos + targetColumn - 1
                                              })) 
                               filledLines

filledColumnsOfALine :: Piece -> Int -> [Int]
filledColumnsOfALine origPiece line = case inBounds 0 (getGridSize origPiece - 1) line of
    False -> []
    True -> map fst (filter (\pair -> snd pair == Filled) (zip [0..] (pieceMatrix !! line)))
    where   
        pieceMatrix = getMatrix (getGrid origPiece)

filledLinesOfaColumn :: Piece -> Int -> [Int]
filledLinesOfaColumn origPiece col = case (inBounds 0 (getGridSize origPiece - 1) col) of
    False -> []
    True -> map fst (filter (\pair -> (snd pair) !! col == Filled) (zip [0..] (getMatrix (getGrid origPiece))))

rightmostFilledColumn :: Piece -> Int
rightmostFilledColumn origPiece =
    fromMaybe (error "peça sem nenhuma célula preenchida - não deveria acontecer")
              (find columnHasFilled [maxCol, maxCol - 1 .. 0])
    where
        pieceMatrix = getMatrix (getGrid origPiece)
        maxCol = getGridSize origPiece - 1
        columnHasFilled col = any (\row -> row !! col == Filled) pieceMatrix

leftMostFilledColumn :: Piece -> Int
leftMostFilledColumn origPiece = 
    fromMaybe (error "peça sem nenhuma célula preenchida - não deveria acontecer")
              (find columnHasFilled [0, 1.. maxCol])
    where
        pieceMatrix = getMatrix (getGrid origPiece)
        maxCol = getGridSize origPiece - 1
        columnHasFilled col = any (\row -> row !! col == Filled) pieceMatrix

bottomMostFilledLine :: Piece -> Int
bottomMostFilledLine origPiece =
    fromMaybe (error "peça sem nenhuma célula preenchida - não deveria acontecer")
              (find lineHasFilled [maxLin, maxLin - 1 .. 0])
    where
        pieceMatrix = getMatrix (getGrid origPiece)
        maxLin = getGridSize origPiece - 1
        lineHasFilled line = any (== Filled) (pieceMatrix !! line)

checkWallCollisionRight :: Piece -> GameBoard -> Bool
checkWallCollisionRight origPiece origBoard = 
    (getWidthPos $ getPosition $ origPiece) + rightmostFilledColumn origPiece + 1 >= getBoardWidth origBoard

checkFloorCollision :: Piece -> GameBoard -> Bool
checkFloorCollision origPiece origBoard = 
    (getHeightPos (getPosition origPiece) + bottomMostFilledLine origPiece) + 1 >= getBoardTotalHeight origBoard

checkWallCollisionLeft :: Piece -> Bool
checkWallCollisionLeft origPiece = 
    (getWidthPos $ getPosition $ origPiece) + leftMostFilledColumn origPiece - 1 < 0


checkIfPieceLegal :: GameBoard -> Piece -> Bool
checkIfPieceLegal origBoard origPiece = 
    not (any isOccupied absolutePositions)
    where
        pieceHeight = getHeightPos (getPosition origPiece)
        pieceWidth  = getWidthPos (getPosition origPiece)
        absolutePositions = 
            [ Position { getHeightPos = pieceHeight + lineIdx, getWidthPos = pieceWidth + colIdx }
            | (lineIdx, colIdx) <- filledCellsWithOffsets origPiece
            ]
        isOccupied pos = accessPosInBoard origBoard pos /= Empty