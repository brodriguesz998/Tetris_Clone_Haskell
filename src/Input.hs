module Input
    ( InputAction(..)
    , inputAction
    ) where

import Data.Char (toLower)
import Graphics.Vty (Event(..), Key(..))

data InputAction
    = MoveLeft
    | MoveRight
    | Rotate
    | SoftDrop
    | Quit
    deriving (Eq, Show)

-- | Holding S or Down produces repeated terminal key events, which gives the
-- game its soft-drop behavior.
inputAction :: Event -> Maybe InputAction
inputAction event = case event of
    EvKey KLeft []  -> Just MoveLeft
    EvKey KRight [] -> Just MoveRight
    EvKey KUp []    -> Just Rotate
    EvKey KDown []  -> Just SoftDrop
    EvKey KEsc []   -> Just Quit
    EvKey (KChar key) _ -> charAction (toLower key)
    _ -> Nothing
  where
    charAction 'a' = Just MoveLeft
    charAction 'd' = Just MoveRight
    charAction 'w' = Just Rotate
    charAction 's' = Just SoftDrop
    charAction 'q' = Just Quit
    charAction _   = Nothing
