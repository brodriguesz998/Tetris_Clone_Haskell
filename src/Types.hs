module Types (module Types) where

data CellState = Filled | Empty | OutOfBounds

data Grid = Grid [[CellState]] 

data Position = Position { getHeightPos :: Int, getWidthPos :: Int}

changeCellState :: CellState -> CellState
changeCellState Filled = Empty
changeCellState Empty = Filled
changeCellState OutOfBounds = OutOfBounds

getMatrix :: Grid -> [[CellState]]
getMatrix (Grid matrix) = matrix 

accessPosInGrid :: Grid -> Position -> CellState --função parcial unsafe, ela será usada como parte de outras funções que fazem a checagem
accessPosInGrid (Grid matrix) origPos = (matrix !!  getWidthPos origPos) !! getHeightPos origPos 

soma1 :: Int -> Int
soma1 x = x + 1
