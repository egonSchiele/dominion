{-# LANGUAGE TemplateHaskell #-}
module Player where
import qualified Card as C
import Control.Lens

data Player = Player {
                _name :: String,
                _deck :: [C.Card],
                _discard :: [C.Card]
} deriving Show

makeLenses ''Player
