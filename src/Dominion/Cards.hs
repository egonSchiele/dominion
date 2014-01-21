module Dominion.Cards (
  module Dominion.Cards.Base,
  module Dominion.Cards.Original,
  module Dominion.Cards
) where

import Dominion.Cards.Base
import Dominion.Cards.Original

-- | All action cards available for use.
allCards = originalCards
