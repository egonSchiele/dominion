{-# LANGUAGE TemplateHaskell #-}

module GameState where
import Control.Lens
import qualified Player as P
import qualified Card as C

data GameState = GameState {
                    _players :: [P.Player],
                    _cards :: [C.Card]
} deriving Show

makeLenses ''GameState
  
