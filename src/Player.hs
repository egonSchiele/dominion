{-# LANGUAGE TemplateHaskell #-}
module Player where
import qualified Card as C
import Control.Lens

data Player = Player {
                _name :: String,
                _deck :: [C.Card],
                _discard :: [C.Card],
                _hand :: [C.Card],
                _actions :: Int,
                _buys :: Int,
                _extraMoney :: Int
} deriving Show

makeLenses ''Player
