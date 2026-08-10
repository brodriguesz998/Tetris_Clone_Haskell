module Main where
import Piece
import Board
import Collision
import Types

boardRows :: [[CellState]]
boardRows =
    [ if r == 7 || r == 8
        then [ if c == 7 then Filled else Empty | c <- [0..9] ]
        else replicate 10 Empty
    | r <- [0..21] ]

testBoard :: GameBoard
testBoard = GameBoard
    { getBoardHeight = 20
    , getBoardTotalHeight = 22
    , getBoardWidth  = 10
    , getBoardGrid   = Grid boardRows
    }

-- ===== Peça 1: I-piece, Deg0, encostada bem na parede direita =====
-- usedSpace I = [[E,E,E,E],[F,F,F,F],[E,E,E,E],[E,E,E,E]]
-- Linha do meio inteira preenchida -> rightmostFilledColumn = 3 (última coluna da grade)
-- Colocando em getWidthPos = 6: ocupa colunas absolutas 6,7,8,9 -> já encostada na parede (col 9 é a última válida)
iPieceDeg0 :: Piece
iPieceDeg0 = Piece
    { getKind     = I
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 2, getWidthPos = 6 }
    , getGrid     = usedSpace I
    , getHeight   = 1
    , getWidth    = 4
    }

iPieceDeg90 :: Piece
iPieceDeg90 = rotatePieceR iPieceDeg0


-- ===== Peça 2: O-piece encostada na pilha (deveria estar obstruída, não pela parede) =====
-- usedSpace O = [[F,F],[F,F]] (2x2, tudo preenchido)
-- Position (5,3): ocupa linhas 5-6, colunas 3-4 -> longe da parede, mas colada
-- nas células preenchidas do tabuleiro em (5,5) e (6,5)
oPieceNextToStack :: Piece
oPieceNextToStack = Piece
    { getKind     = O
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 7, getWidthPos = 5 }
    , getGrid     = usedSpace O
    , getHeight   = 2
    , getWidth    = 2
    }

oPieceBetween :: Piece
oPieceBetween = Piece
    { getKind     = O
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 8, getWidthPos = 8}
    , getGrid     = usedSpace O
    , getHeight   = 2
    , getWidth    = 2
    }

oPieceBetween2 :: Piece
oPieceBetween2 = Piece
    { getKind     = O
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 9, getWidthPos = 8}
    , getGrid     = usedSpace O
    , getHeight   = 2
    , getWidth    = 2
    }

oPieceBetween3 :: Piece
oPieceBetween3 = Piece
    { getKind     = O
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 7, getWidthPos = 8}
    , getGrid     = usedSpace O
    , getHeight   = 2
    , getWidth    = 2
    }

oPieceOpenSpace :: Piece
oPieceOpenSpace = Piece
    { getKind     = O
    , getRotation = Deg0
    , getPosition = Position { getHeightPos = 17, getWidthPos = 2 }
    , getGrid     = usedSpace O
    , getHeight   = 2
    , getWidth    = 2
    }
    

rotateAuxTest :: Grid  -> Grid
rotateAuxTest origGrid = Grid $ reverse $ aux1 $ getMatrix $ origGrid
    where
        aux1 :: [[CellState]] -> [[CellState]]
        aux1[] = [] --erro, nunca deeveria acontecer, pode ser ponto de mlhora no futuro, fazer um tipo erro e etc
        aux1 [lastLine] = foldr (\x -> (++) [[x]]) [] lastLine
        aux1 (firstL : otherL) = zipWith (++) (foldr (\x -> (++) [[x]]) [] firstL) (aux1 otherL)  

main :: IO ()
main = do 
    let oblock = usedSpace(O)
    let iblock = usedSpace(I)
    let jblock = usedSpace(J)
    let lblock = usedSpace(L)
    let sblock = usedSpace(S)
    let zblock = usedSpace(Z)
    let tblock = usedSpace(T)
    print (iblock)
    print (rotateAuxTest iblock)
    print (rotateAuxTest $ rotateAuxTest iblock)
    print (rotateAuxTest $ rotateAuxTest $ rotateAuxTest iblock)
    print (rotateAuxTest $ rotateAuxTest $ rotateAuxTest $ rotateAuxTest iblock)

    print(getBoardGrid testBoard)
    putStrLn "--- Teste 1: I-piece Deg0 encostada na parede direita ---"
    print (canMoveRight iPieceDeg0 testBoard)
    putStrLn "esperado: False (não pode mais mover pra direita)"

    putStrLn "\n--- Teste 2: mesma peça, mas rotacionada (Deg90) ---"
    print (canMoveRight iPieceDeg90 testBoard)
    putStrLn "esperado: True (rotacionar abriu espaço pra mover)"

    putStrLn "\n--- Teste 3: O-piece encostada numa pilha já existente ---"
    print (canMoveRight oPieceNextToStack testBoard)
    putStrLn "esperado: False (bloqueada por célula preenchida do tabuleiro, não pela parede)"

    putStrLn "\n--- Teste 4: O-piece em espaço livre ---"
    print (canMoveRight oPieceOpenSpace testBoard)
    putStrLn "esperado: True (nada bloqueando, pode mover)"

    putStrLn "\n--- Teste 5: O-piece bloqueado pela esquerda por uma peça e pela direita por uma parede ---"
    print (canMoveRight oPieceBetween testBoard)
    print (canMoveLeft oPieceBetween testBoard)
    putStrLn "esperado: False False (Bloqueado dos dois lados)"
    
    putStrLn "\n--- Teste 6: O-piece bloqueado pela esquerda por uma peça e pela direita por uma parede ---"
    print (canMoveRight oPieceBetween2 testBoard)
    print (canMoveLeft oPieceBetween2 testBoard)
    putStrLn "esperado: False True (O está diretamente abaixo da peça)"

    putStrLn "\n--- Teste 7: O-piece bloqueado pela esquerda por uma peça e pela direita por uma parede ---"
    print (canMoveRight oPieceBetween3 testBoard)
    print (canMoveLeft oPieceBetween3 testBoard)
    putStrLn "esperado: False True (Linha de baixo ainda bloqueada pela peça)"

    print (getGrid $ fixPiece $ iPieceDeg0)
    print (getGrid $ fixPiece $ iPieceDeg90)








