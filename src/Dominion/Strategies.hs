module Dominion.Strategies where
import Dominion
import Dominion.Cards
import Control.Monad

-- the big money strategy
bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]

-- -- big money but also buy a smithy whenever you can
-- bigMoneySmithy playerId = do
--     playerId `plays` smithy
--     playerId `buysByPreference` [province, gold, duchy, smithy, silver, copper]

-- market playerId = do
--     playerId `playsByPreference` [market]
--     playerId `buysByPreference` [province, gold, market, duchy, silver, copper]

-- villageIdiot playerId = do
--     playerId `playsByPreference` [village]
--     playerId `buysByPreference` [province, gold, duchy, village, silver, copper]

-- lab playerId = do
--     playerId `playsByPreference` [laboratory]
--     playerId `buysByPreference` [province, gold, laboratory, duchy, silver, copper]

-- festival playerId = do
--     playerId `playsByPreference` [festival]
--     playerId `buysByPreference` [province, gold, festival, duchy, silver, copper]

-- councilRoom playerId = do
--     playerId `playsByPreference` [councilRoom]
--     playerId `buysByPreference` [province, gold, councilRoom, duchy, silver, copper]

-- throneRoom playerId = do
--     playerId `plays` throneRoom `with` (ThroneRoom market)
--     playerId `buysByPreference` [province, gold, market, duchy, throneRoom, silver, copper]

-- multiThroneRoom playerId = do
--     playerId `plays` throneRoom `with` (ThroneRoom throneRoom) `withMulti` [ThroneRoom market, ThroneRoom market]
--     playerId `buysByPreference` [province, gold, market, duchy, throneRoom, silver, copper]
