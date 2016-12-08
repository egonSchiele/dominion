import Dominion
import Dominion.Cards
import Dominion.Strategies
import Control.Monad hiding (join)
import qualified Dominion.Types as T

chapelStrategy p = do
    chapels <- countNum p chapel
    money <- handValue p
    currentRound <- getRound

    -- buy a chapel to start
    when (chapels == 0 && money <= 3) $ do
      p `buys` chapel
      return ()

    p `plays` laboratory
    p `plays` councilRoom

    -- if you have a chapel, use it
    hasChapel <- p `has` chapel
    when (hasChapel && money < 6) $ do
      p `plays` chapel `with` (T.Chapel $ (4 `cardsOf` estate) ++ (2 `cardsOf` copper))
      return ()

    if currentRound < 8
      then p `buysByPreference` [province, gold, laboratory, councilRoom, silver]
      else p `buysByPreference` [province, gold, duchy, silver, estate]

main = dominionWithOpts [Cards [chapel, laboratory, councilRoom, smithy], Iterations 5000] ["adit" `uses` chapelStrategy, "maggie" `uses` bigMoneySmithy]
