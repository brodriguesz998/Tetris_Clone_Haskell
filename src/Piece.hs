module Piece where

newtype Position = Position (Int, Int)
data Rotation =  Deg0 | Deg90 | Deg180 | Deg270 
data CellState = Filled | Empty --por enquanto é só um Bool mas talvez eu expanda a lógica então estou criando essa estrutura
data Grid = Grid [[CellState]] 

data PieceKind = O|B|C|R|S|T|H 

data Piece = Piece {
    getKind :: PieceKind,
    getRotation :: Rotation,
    getPosition :: Position,
    getGrid :: Grid
}

usedSpace :: PieceKind -> Grid
usedSpace O = Grid[[Empty, Filled],[Empty, Filled],[Empty, Filled],[Filled, Filled]]
usedSpace B = Grid[[Filled , Filled],[Empty, Filled],[Empty, Filled],[Empty, Filled]]
usedSpace C = Grid[[Filled, Empty],[Filled, Filled],[Empty, Filled]]
usedSpace R = Grid[[Empty, Filled],[Filled, Filled],[Filled, Empty]]
usedSpace S = Grid[[Filled, Filled],[Filled, Filled]]
usedSpace T = Grid[[Empty, Filled],[Filled, Filled], [Empty,Filled]]
usedSpace H = Grid[[Empty, Filled],[Empty, Filled], [Empty,Filled], [Empty, Filled]]

-- vamos tentar generalizar a operação de rotação de uma peça representada por um grid
-- O0deg = Grid[[Empty, Filled],[Empty, Filled],[Empty, Filled],[Filled, Filled]] 
-- O90deg = Grid[[Filled, Filled, Filled, Filled], [Empty, Empty, Empty, Filled]]
-- O180deg = Grid[[Filled, Filled],[Filled, Empty],[Filled, Empty],[Filled, Empty]]
-- O270deg = Grid[[Filled,Empty,Empty,Empty][Filled,Filled,Filled,Filled]]

--T0deg = Grid[[Empty,Filled],[Filled,Filled],[Empty,Filled]]
--T90deg = Grid[[Filled,Filled,Filled],[Empty,Filled,Empty]]
--T180deg = Grid[[Filled,Empty],[Filled,Filled],[Filled,Empty]]
--T270deg = Grid[[Empty,Filled,Empty],[Filled,Filled,Filled]]


-- peça na "horizontal" para peça na "vertical" : 
-- a linha x da matriz rotacionada é formada pela junção dos y-x elementos de cada linha da
-- matriz original aonde y é o tamanho da linha da matriz original e seguindo a ordem de direita pra esquerda
-- peça na "vertical" para peça na "horizontal" :
-- A n° linha da matriz rotacionada é formada pelo n° elemento de cada linha da matriz original


changeCellState :: CellState -> CellState --por enquanto é só um not lógico mas pode ser que haja mais lógica depois
changeCellState Filled = Empty
changeCellState Empty = Filled

isRotationVertical :: Rotation -> Bool
isRotationVertical Deg0 = False
isRotationVertical Deg180 = False
isRotationVertical _ = True

rotationInc :: Rotation -> Rotation
rotationInc Deg0   = Deg90 
rotationInc Deg90  = Deg180 
rotationInc Deg180  = Deg270 
rotationInc Deg270  = Deg0 
rotationDec:: Rotation -> Rotation
rotationDec Deg0  = Deg270
rotationDec Deg90  = Deg0
rotationDec Deg180  = Deg90
rotationDec Deg270  = Deg180

rotatePiece :: Piece -> Piece
rotatePiece origPiece = Piece {
    getKind = getKind origPiece,
    getRotation =  rotationInc (getRotation origPiece),
    getPosition = getPosition origPiece,
    getGrid = rotateAuxR getGrid origPiece
}
    where rotateAuxR = 