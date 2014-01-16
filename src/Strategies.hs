module Strategies where
import qualified Dominion as D
import qualified Card as C
import qualified Player as P

-- the big money strategy
bigMoney playerId = playerId `D.buysByPreference` [C.province, C.gold, C.duchy, C.silver, C.copper]

-- big money but also buy a smithy whenever you can
bigMoneySmithy playerId = do
    playerId `D.plays` C.smithy
    playerId `D.buysByPreference` [C.province, C.gold, C.duchy, C.smithy, C.silver, C.copper]

market playerId = do
    playerId `D.plays` C.market
    playerId `D.buysByPreference` [C.province, C.gold, C.market, C.duchy, C.silver, C.copper]
