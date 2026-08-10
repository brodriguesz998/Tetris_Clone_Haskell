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
    | Quit
    deriving (Eq, Show)

-- | Translate only the controls the game supports.  There is deliberately no
-- downward movement key: falling is exclusively controlled by gravity ticks.
inputAction :: Event -> Maybe InputAction
inputAction event = case event of
    EvKey KLeft []  -> Just MoveLeft
    EvKey KRight [] -> Just MoveRight
    EvKey KUp []    -> Just Rotate
    EvKey KEsc []   -> Just Quit
    EvKey (KChar key) _ -> charAction (toLower key)
    _ -> Nothing
  where
    charAction 'a' = Just MoveLeft
    charAction 'd' = Just MoveRight
    charAction 'w' = Just Rotate
    charAction 'q' = Just Quit
    charAction _   = Nothing
