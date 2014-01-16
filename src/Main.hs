import qualified Dominion as D
import qualified Player as P
import qualified Card as C
import Control.Monad.State hiding (forM_, forM)
import Control.Concurrent
import Control.Lens
import Control.Arrow
import Text.Printf
import Data.List
import System.Environment
import Control.Monad
import Utils
import qualified Strategies as S
import qualified GameState as G

makePlayer :: String -> P.Player
makePlayer name = P.Player name [] (2 `cardsOf` C.throneRoom ++ 2 `cardsOf` C.market ++ 3 `cardsOf` C.copper ++ 3 `cardsOf` C.estate) [] 1 1 0

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

countPoints :: P.Player -> Int
countPoints player = sum $ map countValue effects
    where cards        = player ^. P.deck ++ player ^. P.discard
          victoryCards = filter (\card -> C.Victory `elem` (card ^. C.cardType)) cards
          effects      = concatMap C._effects victoryCards
          countValue (C.VPValue x) = x
          countValue _ = 0

game :: StateT G.GameState IO ()
game = do
         state <- get
         forM_ (zip (state ^. G.players) [0..]) $ \(p, p_id) -> if (p ^. P.name == "adit")
                                                                then D.playTurn p_id S.throneRoom
                                                                else D.playTurn p_id S.bigMoneySmithy

run :: G.GameState -> IO String
run state = do
              let [p1, p2] = state ^. G.players
              (_, newState) <- runStateT game state
              let cards = newState ^. G.cards
              if gameOver cards
                then do
                  let results = map (id &&& countPoints) (newState ^. G.players) :: [(P.Player, Int)]
                  -- putStrLn "Game over!"
                  -- forM_ results $ \(player, points) -> putStrLn $ printf "player %s got %d points" (player ^. P.name) points
                  return $ P._name . fst $ foldl1 (\(saved_player, max_points) (player, points) -> if points > max_points
                                                                           then (player, points)
                                                                           else (saved_player, max_points)) results
                else do
                  -- threadDelay $ 1 * 500
                  run newState

main = do
    args <- getArgs
    let iterations = case args of
                       [iterations_] -> read iterations_ :: Int
                       _ -> 1
    results <- forM [1..iterations] $ \i -> if even i
                                        then do
                                          run $ G.GameState players cards
                                        else do
                                          run $ G.GameState (reverse players) cards
    forM_ players $ \player -> putStrLn $ printf "player %s won %d times" (player ^. P.name) (count (player ^. P.name) results)
