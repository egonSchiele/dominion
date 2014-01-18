module Dominion.Cards.Base where
import Dominion.Types
-- Treasure cards
copper = Card "Copper" 0 [Treasure] [CoinValue 1]
silver = Card "Silver" 3 [Treasure] [CoinValue 2]
gold = Card "Gold" 6 [Treasure] [CoinValue 3]

-- Victory point cards
estate = Card "Estate" 2 [Victory] [VPValue 1]
duchy = Card "Duchy" 5 [Victory] [VPValue 3]
province = Card "Province" 8 [Victory] [VPValue 6]
curse = Card "Curse" 0 [Victory] [VPValue (-1)]
