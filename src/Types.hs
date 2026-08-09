module Types (module Types) where

data CellState = Fixed |Filled | Empty | OutOfBounds deriving (Eq, Show)

data Grid = Grid [[CellState]] 
instance Show Grid where
    show (Grid rows) = unlines (map showRow rows)
        where
            showRow row = concatMap showCell row
            showCell Filled = "[O]"
            showCell Empty  = "[.]"
            showCell OutOfBounds = "[ERROR]"
            showCell Fixed = "[#]"

data Position = Position { getHeightPos :: Int, getWidthPos :: Int}

inBounds :: Int -> Int -> Int -> Bool
inBounds minVal maxVal x = x >= minVal && x <= maxVal

changeCellState :: CellState -> CellState
changeCellState Filled = Empty
changeCellState Empty = Filled
changeCellState OutOfBounds = OutOfBounds
changeCellState Fixed = Fixed

getMatrix :: Grid -> [[CellState]]
getMatrix (Grid matrix) = matrix 

accessPosInGrid :: Grid -> Position -> CellState --função parcial unsafe, ela será usada como parte de outras funções que fazem a checagem
accessPosInGrid (Grid matrix) origPos = (matrix !! getHeightPos origPos) !! getWidthPos origPos
