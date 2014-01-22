import Test.Hspec
import Dominion
import Dominion.Cards
import qualified Dominion.Types as T
import Control.Applicative

bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]
stupidStrategy playerId = playerId `buysByPreference` [province, gold]

bigMoney2 playerId = do
    roundNum <- getRound
    if (roundNum < 6)
      then playerId `buysByPreference` [province, gold, silver]
      else bigMoney playerId

bigMoneySmithy playerId = do
    playerId `plays` smithy
    roundNum <- getRound
    if (roundNum < 6)
      then playerId `buysByPreference` [province, gold, smithy, silver]
      else bigMoney playerId

io = flip shouldReturn
count x list = realToFrac . length $ filter (==x) list

main = do
    hspec $ do
      describe "integration tests" $ do
        it "the stupid strategy of only buying provinces and golds should never win" $ do
          io True $ do
            winners <- map T.winner <$> dominion ["sherlock" `uses` bigMoney, "watson" `uses` stupidStrategy]
            return $ not ("watson" `elem` winners)

        it "bigMoney2 should win 1.4 times as often as bigMoney" $ do
          io True $ do
            winners <- map T.winner <$> dominion ["sherlock" `uses` bigMoney, "watson" `uses` bigMoney2]
            return $ (count "watson" winners) >= (1.4 * (count "sherlock" winners))

        it "bigMoneySmithy should win 1.4 times as often as bigMoney2" $ do
          io True $ do
            winners <- map T.winner <$> dominionWithOpts [Cards [smithy]] ["sherlock" `uses` bigMoneySmithy, "watson" `uses` bigMoney2]
            return $ (count "sherlock" winners) >= (1.4 * (count "watson" winners))
