{-# LANGUAGE TemplateHaskell #-}

module Dominion.Types (
  -- | This module uses the `Lens` library. So you might notice that the
  -- fields for the constructors look strange: they all have underscores.
  -- Given a card, you can see the cost like this:
  --
  -- > _cost card
  --
  -- But you can also use a lens:
  --
  -- > card ^. cost
  --
  -- The lens library is very useful for modifying deeply nested data
  -- structures, and it's been very useful for this module.
  module Dominion.Types
) where
import           Control.Lens
import           Control.Monad.State
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
              _name     :: String,
              _cost     :: Int,
              _cardType :: [CardType],
              _effects  :: [CardEffect]
} deriving (Show, Eq)
makeLenses ''Card

-- | Used with the `thief` card.
data ThiefTrashAction = TrashOnly Card | GainTrashedCard Card

-- | Some cards have a followup action associated with them. For example,
-- when you play a `workshop`, you need to choose what card you're going to
-- get. To use the followup action, you need to use the relevant data
-- constructor. See the documentation for each card to find out how to use
-- each type of `FollowupAction`.
data FollowupAction = ThroneRoom Card
                    -- | Takes a list of cards to discard.
                    | Cellar [Card]
                    -- | Boolean value representing whether you want to
                    -- move your deck into the discard pile.
                    | Chancellor Bool
                    -- | Takes a list of cards to trash.
                    | Chapel [Card]
                    -- | Takes the card you want to gain.
                    | Feast Card
                    -- | Takes the card you want to trash.
                    | Mine Card
                    -- | The first card is the card you are trashing, the
                    -- second card is the card you are gaining.
                    | Remodel (Card, Card)
                    -- | The first element is the list of cards you would discard for yourself,
                    -- the second is the lsit of cards you want others to discard.
                    | Spy ([Card], [Card])
                    -- | The function gets a list of treasure cards.
                    -- had. You return either `TrashOnly` to have the player
                    -- trash a card, or `GainTrashedCard` to gain the trashed
                    -- card. This function is called for every other
                    -- player in the game.
                    | Thief ([Card] -> ThiefTrashAction)
                    -- | Takes the card you want to gain.
                    | Workshop Card

---------------------------
-- PLAYER
---------------------------

data Player = Player {
                _playerName :: String,
                _deck       :: [Card],
                _discard    :: [Card],
                _hand       :: [Card],
                _actions    :: Int,
                _buys       :: Int,
                -- | Extra money gained from an action card (like +1 money
                -- from market).
                _extraMoney :: Int
} deriving Show
makeLenses ''Player

type PlayerId = Int

---------------------------
-- GAME STATE
---------------------------

-- | This is what keeps track of all the state in the whole game.
-- Get the round number like this:
--
-- > state <- get
-- > let roundNum = state ^. round
data GameState = GameState {
                    _players :: [Player],
                    -- | list of all the cards still in play.
                    _cards   :: [Card],
                    -- | round number
                    _round   :: Int,
                    _verbose :: Bool
} deriving Show
makeLenses ''GameState

-- The Dominion monad is just the `StateT` monad that has a `GameState`
-- plus the IO monad.
type Dominion a = StateT GameState IO a

-- | Given a playerId, run some actions for this player. Example:
--
-- > bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]
type Strategy = PlayerId -> Dominion ()

-- | When you use a card (either you play it or you buy something),
-- you get a `PlayResult`. A `PlayResult` is either a `Left` with an error message,
-- or a `Right` with a value.
type PlayResult a = Either String a

-- | When you play an action card that needs a decision on your part,
-- `plays` will return a Followup.
type Followup = (PlayerId, CardEffect)

-- | You can set these options if you use `dominionWithOpts`. Example:
--
-- > main = dominionWithOpts [Iterations 1, Log True, Cards [smithy]] ...
data Option =
            -- | Number of iterations to run.
            Iterations Int
            -- | Enable logging
            | Log Bool
            -- | A list of cards that you definitely want in the game.
            -- Useful if you are testing a strategy that relies on
            -- a particular card.
            | Cards [Card]
            deriving (Show)

-- | Each `PlayerResult` is a tuple of a player and their final score.
type PlayerResult = (Player, Int)

-- | Players and their scores.
data Result = Result {
            playerResults :: [PlayerResult],
            winner        :: String
            } deriving (Show)
