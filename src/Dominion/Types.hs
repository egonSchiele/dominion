{-# LANGUAGE TemplateHaskell #-}

module Dominion.Types where
import Control.Lens
import Control.Monad.State
---------------------------
-- CARD
---------------------------

data CardType = Action
              | Attack
              | Reaction
              | Treasure
              | Victory
              | Duration
              deriving (Show, Eq)

data CardEffect = CoinValue Int
                | VPValue Int
                | PlusDraw Int
                | PlusCoin Int
                | PlusBuy Int
                | PlusAction Int
                | Trash Int
                | DurationDraw Int
                | DurationAction Int
                | DurationCoin Int
                | DurationBuy Int
                | PlayActionCard Int
                deriving (Show, Eq)

data Card = Card {
              _name :: String,
              _cost :: Int,
              _cardType :: [CardType],
              _effects :: [CardEffect]
} deriving (Show, Eq)
makeLenses ''Card

data ExtraEffect = ThroneRoom Card

---------------------------
-- PLAYER
---------------------------

data Player = Player {
                _playerName :: String,
                _deck :: [Card],
                _discard :: [Card],
                _hand :: [Card],
                _actions :: Int,
                _buys :: Int,
                _extraMoney :: Int
} deriving Show
makeLenses ''Player

type PlayerId = Int

---------------------------
-- GAME STATE
---------------------------

data GameState = GameState {
                    _players :: [Player],
                    _cards :: [Card],
                    _round :: Int,
                    _verbose :: Bool
} deriving Show
makeLenses ''GameState

-- the Dominion monad is just the state monad that has some state
-- about the gameplay, plus it has the IO monad. a is the return
-- value of whatever function.
type Dominion a = StateT GameState IO a
type Strategy = PlayerId -> Dominion ()

data Option = Iterations Int | Log Bool
