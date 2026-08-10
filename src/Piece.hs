module Piece where
import Types
import System.Random (StdGen, randomR)


data Rotation =  Deg0 | Deg90 | Deg180 | Deg270
    deriving (Show, Eq)
data PieceKind = O | J | L | I | S | Z | T
    deriving (Show, Eq, Enum, Bounded)
data Piece = Piece {
    getKind :: PieceKind,
    getRotation :: Rotation,
    getPosition :: Position,
    getGrid :: Grid
}

getGridSize :: Piece -> Int
getGridSize = pieceSize . getKind

pieceSize :: PieceKind -> Int
pieceSize O = 2
pieceSize I = 4
pieceSize _ = 3

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

randomPieceKindPure :: StdGen -> (PieceKind, StdGen)
randomPieceKindPure gen =
    let (n, newGen) = randomR (fromEnum (minBound :: PieceKind), fromEnum (maxBound :: PieceKind)) gen
    in (toEnum n, newGen)

fixPiece :: Piece -> Piece
fixPiece origPiece =
    origPiece{getGrid = Grid $ map (\list -> map (\cell -> if cell == Filled then Fixed else cell) list) (getMatrix $ getGrid origPiece)}

rotateAuxR :: Piece  -> Grid
rotateAuxR origPiece = Grid $ reverse $ aux1 $ getMatrix $ getGrid $ origPiece
    where
        aux1 :: [[CellState]] -> [[CellState]]
        aux1[] = [] --erro, nunca deeveria acontecer, pode ser ponto de mlhora no futuro, fazer um tipo erro e etc
        aux1 [lastLine] = foldr (\x -> (++) [[x]]) [] lastLine
        aux1 (firstL : otherL) = zipWith (++) (foldr (\x -> (++) [[x]]) [] firstL) (aux1 otherL)

rotatePieceR :: Piece -> Piece
rotatePieceR origPiece = origPiece {
    getRotation =  rotationInc (getRotation origPiece),
    getGrid = rotateAuxR origPiece
}
