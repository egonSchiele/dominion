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
                | PlusCard Int
                | PlusCoin Int
                | PlusBuy Int
                | PlusAction Int
                | DurationDraw Int
                | DurationAction Int
                | DurationCoin Int
                | DurationBuy Int
                | TrashCards Int
                | TrashThisCard
                | GainCardUpto Int
                | PlayActionCard Int
                | AdventurerEffect
                | BureaucratEffect
                | CellarEffect
                | ChancellorEffect
                | GardensEffect
                | LibraryEffect
                | MineEffect
                | MoneylenderEffect
                | RemodelEffect
                | SpyEffect
                | ThiefEffect
                | OthersPlusCard Int
                | OthersDiscardTo Int
                | OthersGainCurse Int
                deriving (Show, Eq)

data Card = Card {
              _name :: String,
              _cost :: Int,
              _cardType :: [CardType],
              _effects :: [CardEffect]
} deriving (Show, Eq)
makeLenses ''Card

data FollowupAction = ThroneRoom Card
                    | Cellar [Card]
                    | Chancellor Bool
                    | Mine Card
                    | Remodel Card
                    -- the first element is the list of cards you would discard for yourself,
                    -- the second is the lsit of cards you want others to discard
                    | Spy ([Card], [Card])
                    | Thief ? -- tricky one

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

-- when you use a card (either you play it or you buy something),
-- you get a play result which is either a Left with an error message,
-- or a Right with a value.
type PlayResult a = Either String a

-- when you play an action card that needs a decision on your part,
-- `play` will return a Followup. This is just the player that needs
-- to followup and the effect they need to follow up on. Then they
-- can follow up using `with` and passing in the necessary data as the
-- FollowupAction.
type Followup = (PlayerId, CardEffect)
data Option = Iterations Int | Log Bool
