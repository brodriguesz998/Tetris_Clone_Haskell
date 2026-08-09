module Collision (module Collision) where
import Piece 
import Board
import Types
import Data.List (find)
import Data.Maybe (fromMaybe)

--aqui eu vou lidar com tudo referente a mudança de posição da peça no tabuleiro, verificar se a peça encostou no chao ou se o jogador perdeu




--accessPosInBoard espera coordenadas absolutas do tabuleiro e filledLinesOfaCollumn e rightmostFilledColumn retornam coordenadas relativas da peça
canMoveRight :: Piece -> GameBoard -> Bool
canMoveRight origPiece origBoard = 
    not (checkWallCollisionRight origPiece origBoard) && not (any (\cell -> cell == Filled) cellsToTheRight)
    where
        origPos = getPosition origPiece
        targetColumn = rightmostFilledColumn origPiece
        filledLines = filledLinesOfaCollumn origPiece targetColumn
        cellsToTheRight = map (\line -> accessPosInBoard origBoard 
                                    (Position { getHeightPos = getHeightPos origPos + line
                                              , getWidthPos = getWidthPos origPos + targetColumn + 1 
                                              })) 
                               filledLines

checkWallCollisionRight :: Piece -> GameBoard -> Bool
checkWallCollisionRight origPiece origBoard = 
    (getWidthPos $ getPosition $ origPiece) + rightmostFilledColumn origPiece + 1 >= getBoardWidth origBoard

inBounds :: Int -> Int -> Int -> Bool
inBounds minVal maxVal x = x >= minVal && x <= maxVal

filledLinesOfaCollumn :: Piece -> Int -> [Int]
filledLinesOfaCollumn origPiece col = case (inBounds 0 (getGridSize origPiece - 1) col) of
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