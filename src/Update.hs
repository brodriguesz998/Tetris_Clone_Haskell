module Update where
import Collision
import Types
import Board
import Piece


--devido a natureza do haskell de não ter efeitos colaterais, a função gameover é apenas uma flag, ela não pode acabar o jogo,
--então eu tenho que ter a cautela de executar a função de dar game over logo depois de que a função movePieceDown for executada infelizmente
movePieceDown :: GameBoard -> GameBoard
movePieceDown origBoard = case canMoveDown curPiece origBoard of
    True -> origBoard {getCurrentPiece = 
        curPiece{getPosition =
             curPos{getHeightPos = curHeight +1 }}}
    False -> fixCurPiece origBoard
    where
        curPiece = getCurrentPiece origBoard
        curPos = getPosition curPiece
        curHeight = getHeightPos curPos

fixCurPiece :: GameBoard -> GameBoard
fixCurPiece origBoard = 
    let fixedGameBoard = fixPieceInBoard origBoard
    in case checkGameOver fixedGameBoard of
        True  -> fixedGameBoard { getGameStatus  = GameOver}
        False -> fixedGameBoard { getCurrentPiece = spawnNewPiece fixedGameBoard }

-- essa função por design não vai escolher uma nova peça, apenas fixar a atual, isso pode ser perigoso então use com cautela
-- ou seja, toda vez que for usar ela e voce sabe que o jogo não for acabar, tenha certeza de criar uma nova peça atual, pois por
-- design não se pode mover uma peça fixada

--primeiro transformar todo Filled do grid do field peça atual do board em Fixed
--depois de fato inserir o grid da peça na matriz do board no lugar correto
 
fixPieceInBoard :: GameBoard -> GameBoard
fixPieceInBoard origBoard = 

spawnNewPiece :: GameBoard -> GameBoard
spawnNewPiece origBoard = origBoard
    { getCurrentPiece = origPiece
        { getKind     = randomKind
        , getRotation = Deg0
        , getPosition = Position { getHeightPos = 0, getWidthPos = 0 }
        , getGrid     = usedSpace randomKind
        , getHeight   = setHeight randomKind
        , getWidth    = setWidth randomKind
        }
    , getRandomGen = newGen
    }
    where
        origPiece = getCurrentPiece origBoard
        (randomKind, newGen) = randomPieceKindPure (getRandomGen origBoard)

checkGameOver :: GameBoard -> Bool
checkGameOver origBoard = any (\y -> any (\x -> x == Fixed) (getMatrix (getBoardGrid origBoard) !! y)) [3,2,1,0]