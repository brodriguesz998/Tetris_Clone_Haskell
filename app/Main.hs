module Main (main) where

import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.AttrMap (attrMap)
import Brick.Main (App(..), customMainWithDefaultVty, halt, neverShowCursor)
import Brick.Types (BrickEvent(..), EventM, modify)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Graphics.Vty.Attributes (defAttr)
import System.Random (newStdGen)

import Board (GameBoard, newGameBoard)
import Input (InputAction(..), inputAction)
import Render (drawGame)
import Update (movePieceLeft, movePieceRight, movePieceDown, rotatePieceInBoard)

data Name = Game
    deriving (Eq, Ord, Show)

data AppEvent = GravityTick

gravityIntervalMicros :: Int
gravityIntervalMicros = 600000

main :: IO ()
main = do
    channel <- newBChan 1
    void . forkIO $ gravityLoop channel
    initialBoard <- newGameBoard <$> newStdGen
    void $ customMainWithDefaultVty (Just channel) tetrisApp initialBoard

gravityLoop :: BChan AppEvent -> IO ()
gravityLoop channel = forever $ do
    threadDelay gravityIntervalMicros
    writeBChan channel GravityTick

tetrisApp :: App GameBoard AppEvent Name
tetrisApp = App
    { appDraw = drawGame
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const (attrMap defAttr [])
    }

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameBoard ()
handleEvent event = case event of
    AppEvent GravityTick -> modify movePieceDown
    VtyEvent vtyEvent -> case inputAction vtyEvent of
        Nothing -> pure ()
        Just Quit -> halt
        Just MoveLeft -> modify movePieceLeft
        Just MoveRight -> modify movePieceRight
        Just Rotate -> modify rotatePieceInBoard
        Just SoftDrop -> modify movePieceDown
    _ -> pure ()
