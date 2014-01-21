module Dominion.Strategies where
import Dominion
import Dominion.Cards
import Control.Monad

-- | Buy the most expensive victory or treasure card you can.
bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]

-- | Same as `bigMoney` but also buy a `smithy` whenever you can.
bigMoneySmithy playerId = do
    playerId `plays` smithy
    playerId `buysByPreference` [province, gold, duchy, smithy, silver, copper]

-- | A strategy that should never win: buy only provinces and golds
-- exclusively.
stupidStrategy playerId = playerId `buysByPreference` [province, gold]
