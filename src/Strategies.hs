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
    playerId `D.playsByPreference` [C.market]
    playerId `D.buysByPreference` [C.province, C.gold, C.market, C.duchy, C.silver, C.copper]

villageIdiot playerId = do
    playerId `D.playsByPreference` [C.village]
    playerId `D.buysByPreference` [C.province, C.gold, C.duchy, C.village, C.silver, C.copper]

lab playerId = do
    playerId `D.playsByPreference` [C.laboratory]
    playerId `D.buysByPreference` [C.province, C.gold, C.laboratory, C.duchy, C.silver, C.copper]

festival playerId = do
    playerId `D.playsByPreference` [C.festival]
    playerId `D.buysByPreference` [C.province, C.gold, C.festival, C.duchy, C.silver, C.copper]

councilRoom playerId = do
    playerId `D.playsByPreference` [C.councilRoom]
    playerId `D.buysByPreference` [C.province, C.gold, C.councilRoom, C.duchy, C.silver, C.copper]

throneRoom playerId = do
    result <- (playerId `D.plays` C.throneRoom)
    case result of
      Left str -> return $ Left str
      Right result_ -> result_ `D.with` (D.ThroneRoom C.market)
    playerId `D.buysByPreference` [C.province, C.gold, C.market, C.duchy, C.throneRoom, C.silver, C.copper]
