import qualified Dominion as D
import qualified Player as P
import qualified Card as C
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Control.Arrow
import Text.Printf
makePlayer :: String -> P.Player
makePlayer name = P.Player name [] (7 `cardsOf` C.copper ++ 3 `cardsOf` C.estate)

cardsOf count card = take count $ repeat card
pileOf card = 10 `cardsOf` card

players = [makePlayer "adit", makePlayer "maggie"]


cards = concatMap pileOf [ C.copper,
                           C.silver,
                           C.gold,
                           C.estate,
                           C.duchy,
                           C.province,
                           C.curse,
                           C.smithy,
                           C.village,
                           C.laboratory,
                           C.festival,
                           C.market,
                           C.woodcutter ]

-- TODO check for 3 piles gone
gameOver cards
    | not (C.province `elem` cards) = True
    | otherwise = False

count x list = length $ filter (==x) list

countPoints player = sum $ map countValue effects
    where cards        = player ^. P.deck ++ player ^. P.discard
          victoryCards = filter (\card -> C.Victory `elem` C.cardType card) cards
          effects      = concatMap C.effects victoryCards
          countValue (C.VPValue x) = x
          countValue _ = 0


run :: D.GameState -> IO ()
run state = do
              let [p1, p2] = state ^. D.players
              -- print (map C.name $ p1 ^. P.deck)
              -- print (map C.name $ p2 ^. P.deck)
              (_, newState) <- runStateT D.game state
              let cards = newState ^. D.cards
              if gameOver cards
                then do
                  putStrLn "Game over!"
                  let results = map (countPoints &&& id) (newState ^. D.players)
                  forM_ results $ \(points, player) -> putStrLn $ printf "player %s got %d points" (player ^. P.name) points
                else do
                  threadDelay $ 1 * 500
                  run newState

main = run $ D.GameState players cards
