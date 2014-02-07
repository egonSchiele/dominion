module Dominion.Cards (
  module Dominion.Cards,
  module X
) where

import Dominion.Cards.Base as X
import Dominion.Cards.Original as X
import Dominion.Cards.Intrigue as X

-- | All action cards available for use.
allActionCards = originalCards ++ intrigueCards
