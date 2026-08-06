module Main where

data CellState = Filled | Empty deriving Show

aux2 :: [CellState] -> [[CellState]]
aux2 [] = []
aux2 (x:xs) = [[x]] ++ aux2 xs

gridTrue :: [[CellState]] -> [[CellState]]
gridTrue [] = [] --erro, nunca deeveria acontecer, pode ser ponto de mlhora no futuro, fazer um tipo erro e etc
gridTrue [lastLine] = aux2 lastLine
gridTrue (firstL : otherL) = zipWith (++) (aux2 firstL) (gridTrue otherL)  


trueReverse :: [[CellState]] -> [[CellState]]
trueReverse x = reverse $ gridTrue x

main :: IO ()
main = do 
    let test1 = [[Empty,Filled],[Empty,Filled],[Filled,Filled]]
    let test2 = [[Filled,Filled,Filled],[Empty,Empty,Filled]]
    let test3 = [[Filled,Filled],[Filled,Empty],[Filled,Empty]]
    let test4 = [[Filled,Empty,Empty],[Filled,Filled,Filled]]
    print $ trueReverse test1
    print $ trueReverse test2
    print $ trueReverse test3
    print $ trueReverse test4




