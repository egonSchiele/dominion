module Dominion.Cards.Intrigue (
  -- | Playing a card is easy:
  --
  -- > playerId `plays` adventurer
  module Dominion.Cards.Intrigue
) where
import Dominion.Types

greatHall = Card "Great Hall" 3 [Victory, Action] [VPValue 1, PlusCard 1, PlusAction 1]

intrigueCards = [greatHall]
