import           Control.Applicative
import           Control.Lens        hiding (has, indices, uses)
import           Control.Monad.State hiding (state)
import           Dominion
import qualified Dominion.Cards      as CA
import           Dominion.Internal
import qualified Dominion.Types      as T
import           Prelude             hiding (log)
import           Test.Hspec

-- | Use this to run a strategy once and inspect the gamestate.
runOnce :: T.Player -> T.Strategy -> IO T.GameState
runOnce player strategy = do
  state <- makeGameState [] [player]
  snd <$> runStateT (game [strategy]) state

-- | Give a hand of cards and a function. You play one round
-- with this given hand of cards. The function returns True or False.
withHand :: [T.Card] -> [T.Option] -> (T.PlayerId -> T.Dominion Bool) -> IO Bool
withHand hand options func = do
    let player = T.Player "testPlayer" (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate) [] hand 1 1 0
    state <- makeGameState options [player]
    fst <$> runStateT (func 0) state

bigMoney playerId = playerId `buysByPreference` [CA.province, CA.gold, CA.duchy, CA.silver, CA.copper]
stupidStrategy playerId = playerId `buysByPreference` [CA.province, CA.gold]

bigMoney2 playerId = do
    roundNum <- getRound
    if (roundNum < 6)
      then playerId `buysByPreference` [CA.province, CA.gold, CA.silver]
      else bigMoney playerId

bigMoneySmithy playerId = do
    playerId `plays` CA.smithy
    roundNum <- getRound
    if (roundNum < 6)
      then playerId `buysByPreference` [CA.province, CA.gold, CA.smithy, CA.silver]
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

        it "bigMoneySmithy should win more often than bigMoney2" $ do
          io True $ do
            winners <- map T.winner <$> dominionWithOpts [Cards [CA.smithy]] ["sherlock" `uses` bigMoneySmithy, "watson" `uses` bigMoney2]
            return $ (count "sherlock" winners) >= (count "watson" winners)

        -- silly spec, shows an example of how to use `withHand`
        it "market should add to the players cards, buys, money, and actions" $ do
          io True $ do
            withHand [CA.market, CA.copper, CA.copper, CA.estate, CA.estate] [] $ \playerId -> do
              playerId `plays` CA.market
              player <- getPlayer playerId
              return $ player ^. T.actions == 1 && player ^. T.extraMoney == 1 && player ^. T.buys == 2
      describe "Feast" $ do
        it "should only trash itself once" $ do
          io True $ do
            withHand (CA.throneRoom : replicate 3 CA.feast) $ \playerId -> do
              playerId `plays` CA.throneRoom `with` (T.ThroneRoom CA.feast) `withMulti` (replicate 2 $ T.Feast CA.duchy)
              player <- getPlayer playerId
              return $ player ^. T.hand == replicate 2 CA.feast && player ^. T.discard == replicate 3 CA.duchy
