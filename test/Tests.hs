module Main (main) where

import Control.Monad (unless)
import System.Random (mkStdGen)

import Board
import Piece
import Types
import Update

main :: IO ()
main = do
    let board = newGameBoard (mkStdGen 1)
        piece = getCurrentPiece board
        afterTick = movePieceDown board
        afterLeft = movePieceLeft board
        fullBottomRow = board
            { getBoardGrid = Grid
                (replicate 23 (replicate 10 Empty) ++ [replicate 10 Fixed])
            }

    assert "the initial piece is visible" (getHeightPos (getPosition piece) >= 3)
    assert "a gravity tick moves the piece exactly one row"
        (getHeightPos (getPosition (getCurrentPiece afterTick)) == 4)
    assert "horizontal input does not move the piece down"
        (getHeightPos (getPosition (getCurrentPiece afterLeft)) == 3)
    assert "horizontal input moves the piece immediately"
        (getWidthPos (getPosition (getCurrentPiece afterLeft)) == 2)
    assert "a completed row is cleared"
        (all (all (== Empty)) (getMatrix (getBoardGrid (clearFullLines fullBottomRow))))

    putStrLn "All tests passed."

assert :: String -> Bool -> IO ()
assert description condition =
    unless condition (ioError (userError ("Test failed: " ++ description)))
