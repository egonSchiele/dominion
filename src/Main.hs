import qualified Dominion as D
import qualified Player as P
import qualified Card as C
import Control.Monad.State
import Control.Concurrent
import Control.Lens

makePlayer :: String -> P.Player
makePlayer name = P.Player name (7 `cardsOf` C.copper ++ 3 `cardsOf` C.estate) []

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

run :: D.GameState -> IO ()
run state = do
              let [p1, p2] = state ^. D.players
              -- print (map C.name $ p1 ^. P.deck)
              -- print (map C.name $ p2 ^. P.deck)
              (_, newState) <- runStateT D.game state
              threadDelay $ 1 * 500 * 1000
              run newState

main = run $ D.GameState players cards
