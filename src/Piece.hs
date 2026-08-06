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
    getGrid :: Grid,
    getHeight :: Int,
    getWidth :: Int
}

getMatrix :: Grid -> [[CellState]]
getMatrix (Grid matrix) = matrix 

setHeight :: PieceKind -> Int
setHeight H = 1
setHeight _ = 2

setWidth :: PieceKind -> Int
setWidth S = 2
setWidth _ = 3

usedSpace :: PieceKind -> Grid
usedSpace O = Grid[[Empty, Filled],[Empty, Filled],[Empty, Filled],[Filled, Filled]]
usedSpace B = Grid[[Filled , Filled],[Empty, Filled],[Empty, Filled],[Empty, Filled]]
usedSpace C = Grid[[Filled, Empty],[Filled, Filled],[Empty, Filled]]
usedSpace R = Grid[[Empty, Filled],[Filled, Filled],[Filled, Empty]]
usedSpace S = Grid[[Filled, Filled],[Filled, Filled]]
usedSpace T = Grid[[Empty, Filled],[Filled, Filled], [Empty,Filled]]
usedSpace H = Grid[[Empty, Filled],[Empty, Filled], [Empty,Filled], [Empty, Filled]]

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

-- vamos tentar generalizar a operação de rotação de uma peça representada por um grid
-- O0deg = Grid[[Empty,Filled],[Empty,Filled],[Filled,Filled]] 
-- O90deg = Grid[[Filled,Filled,Filled],[Empty,Empty,Filled]]
-- O180deg = Grid[[Filled,Filled],[Filled,Empty],[Filled,Empty]]
-- O270deg = Grid[[Filled,Empty,Empty],[Filled,Filled,Filled]]

--T0deg = Grid[[Empty,Filled],[Filled,Filled],[Empty,Filled]]
--T90deg = Grid[[Filled,Filled,Filled],[Empty,Filled,Empty]]
--T180deg = Grid[[Filled,Empty],[Filled,Filled],[Filled,Empty]]
--T270deg = Grid[[Empty,Filled,Empty],[Filled,Filled,Filled]]

aux2 :: [CellState] -> [[CellState]]
aux2 [] = []
aux2 (x:xs) = [[x]] ++ aux2 xs 

rotateAuxR :: Piece  -> Grid
rotateAuxR origPiece = Grid $ reverse $ aux1 $ getMatrix $ getGrid $ origPiece 
    where
        aux1 :: [[CellState]] -> [[CellState]]
        aux1[] = [] --erro, nunca deeveria acontecer, pode ser ponto de mlhora no futuro, fazer um tipo erro e etc
        aux1 [lastLine] = aux2 lastLine
        aux1 (firstL : otherL) = zipWith (++) (aux2 firstL) (aux1 otherL)  
    
rotatePiece :: Piece -> Piece
rotatePiece origPiece = Piece {
    getKind = getKind origPiece,
    getRotation =  rotationInc (getRotation origPiece),
    getPosition = getPosition origPiece,
    getGrid = rotateAuxR origPiece,
    getHeight = getWidth origPiece,  
    getWidth = getHeight origPiece   
}