module Render (drawGame) where

import Brick (Widget, padTop, str, vBox, Padding(..))
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import Board (GameBoard(..), accessPosInBoard, filledCellsWithOffsets)
import Game (GameStatus(..))
import Piece (Piece(..))
import Types (CellState(..), Position(..))

drawGame :: GameBoard -> [Widget name]
drawGame board =
    [ Center.hCenter $
        vBox
            [ Border.borderWithLabel (str " TETRIS ") (vBox boardRows)
            , padTop (Pad 1) (str (statusText board))
            , str "A/Left: move   D/Right: move   W/Up: rotate   S/Down: soft drop   Q/Esc: quit"
            ]
    ]
  where
    visibleStart = getBoardTotalHeight board - getBoardHeight board
    boardRows =
        [ str (renderRow board row)
        | row <- [visibleStart .. getBoardTotalHeight board - 1]
        ]

renderRow :: GameBoard -> Int -> String
renderRow board row =
    concatMap (renderCell board row) [0 .. getBoardWidth board - 1]

renderCell :: GameBoard -> Int -> Int -> String
renderCell board row col
    | (row, col) `elem` currentPieceCells = "[]"
    | accessPosInBoard board (Position row col) == Fixed = "##"
    | otherwise = "  "
  where
    piece = getCurrentPiece board
    piecePosition = getPosition piece
    currentPieceCells =
        [ (getHeightPos piecePosition + pieceRow, getWidthPos piecePosition + pieceColumn)
        | (pieceRow, pieceColumn) <- filledCellsWithOffsets piece
        ]

statusText :: GameBoard -> String
statusText board = case getGameStatus board of
    Playing  -> "Pieces fall every 0.6 seconds."
    GameOver -> "Game over. Press Q or Esc to exit."
