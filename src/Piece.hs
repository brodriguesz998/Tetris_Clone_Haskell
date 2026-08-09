module Piece where
import Types 

data Rotation =  Deg0 | Deg90 | Deg180 | Deg270 
data PieceKind = O|J|L|Z|S|T|I 
data Piece = Piece {
    getKind :: PieceKind,
    getRotation :: Rotation,
    getPosition :: Position,
    getGrid :: Grid,
    getHeight :: Int,
    getWidth :: Int
}

getGridSize :: Piece -> Int
getGridSize pc = case (getKind pc) of
                O -> 2
                I -> 4
                _ ->3

setHeight :: PieceKind -> Int
setHeight I = 1
setHeight _ = 2

setWidth :: PieceKind -> Int
setWidth O = 2
setWidth I = 4
setWidth _ = 3

--nós iremos modelar cada peça em um grid 3x3 e a peça Hero em um grid 4x4, isso é para que tenhamos um ponto de pivo para a rotação centrado, para que a mudança de posição seja feita.
--All tetrominoes spawn horizontally, fully above the playfield.
usedSpace :: PieceKind -> Grid
usedSpace O = Grid[[Filled,Filled],[Filled,Filled]]
usedSpace J = Grid[[Filled,Empty,Empty],[Filled,Filled,Filled],[Empty,Empty,Empty]]
usedSpace L = Grid[[Empty,Empty,Filled],[Filled,Filled,Filled],[Empty,Empty,Empty]]
usedSpace I = Grid[[Empty,Empty,Empty,Empty],[Filled,Filled,Filled,Filled],[Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty]]
usedSpace S = Grid[[Empty,Filled,Filled],[Filled,Filled,Empty],[Empty,Empty,Empty]]
usedSpace Z = Grid[[Filled,Filled,Empty],[Empty,Filled,Filled],[Empty,Empty,Empty]]
usedSpace T = Grid[[Empty,Filled,Empty],[Filled,Filled,Filled],[Empty,Empty,Empty]]

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

-- vamos tentar generalizar a operação de rotação de uma peça representada por um grid
-- O0deg = Grid[[Empty,Filled],[Empty,Filled],[Filled,Filled]] 
-- O90deg = Grid[[Filled,Filled,Filled],[Empty,Empty,Filled]]
-- O180deg = Grid[[Filled,Filled],[Filled,Empty],[Filled,Empty]]
-- O270deg = Grid[[Filled,Empty,Empty],[Filled,Filled,Filled]]

--T0deg = Grid[[Empty,Filled],[Filled,Filled],[Empty,Filled]]
--T90deg = Grid[[Filled,Filled,Filled],[Empty,Filled,Empty]]
--T180deg = Grid[[Filled,Empty],[Filled,Filled],[Filled,Empty]]
--T270deg = Grid[[Empty,Filled,Empty],[Filled,Filled,Filled]]


rotateAuxR :: Piece  -> Grid
rotateAuxR origPiece = Grid $ reverse $ aux1 $ getMatrix $ getGrid $ origPiece 
    where
        aux1 :: [[CellState]] -> [[CellState]]
        aux1[] = [] --erro, nunca deeveria acontecer, pode ser ponto de mlhora no futuro, fazer um tipo erro e etc
        aux1 [lastLine] = foldr (\x -> (++) [[x]]) [] lastLine
        aux1 (firstL : otherL) = zipWith (++) (foldr (\x -> (++) [[x]]) [] firstL) (aux1 otherL)  

rotatePiece :: Piece -> Piece
rotatePiece origPiece = origPiece {
    getRotation =  rotationInc (getRotation origPiece),
    getGrid = rotateAuxR origPiece,
    getHeight = getWidth origPiece,  
    getWidth = getHeight origPiece 
}