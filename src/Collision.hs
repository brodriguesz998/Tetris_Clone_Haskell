module Collision (module Collision) where
import Piece 
import Board
import Types

--aqui eu vou lidar com tudo referente a mudança de posição da peça no tabuleiro, verificar se a peça encostou no chao ou se o jogador perdeu
checkWallCollisionRight :: Piece -> GameBoard -> Bool
checkWallCollisionRight origPiece origBoard = 

checkLastColumn ::  Piece -> Bool
checkLastColumn origPiece = any (\x -> (x !! lastElementIndex) == Filled) getMatrix (getGrid)
    where
        pieceGrid = getGrid origPiece
        lastElementIndex = (getWidth origPiece) - 1