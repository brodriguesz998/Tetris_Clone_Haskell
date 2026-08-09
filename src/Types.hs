module Types (module Types) where

data CellState = Filled | Empty | OutOfBounds deriving (Eq, Show)

data Grid = Grid [[CellState]] 
instance Show Grid where
    show (Grid rows) = unlines (map showRow rows)
        where
            showRow row = concatMap showCell row
            showCell Filled = "[#]"
            showCell Empty  = "[.]"
            showCell OutOfBounds = "[ERROR]"

data Position = Position { getHeightPos :: Int, getWidthPos :: Int}

changeCellState :: CellState -> CellState
changeCellState Filled = Empty
changeCellState Empty = Filled
changeCellState OutOfBounds = OutOfBounds

getMatrix :: Grid -> [[CellState]]
getMatrix (Grid matrix) = matrix 

accessPosInGrid :: Grid -> Position -> CellState --função parcial unsafe, ela será usada como parte de outras funções que fazem a checagem
accessPosInGrid (Grid matrix) origPos = (matrix !! getHeightPos origPos) !! getWidthPos origPos

soma1 :: Int -> Int
soma1 x = x + 1
