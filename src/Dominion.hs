module Dominion where
import qualified Player as P
import qualified Card as C
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Control.Concurrent

for = flip map

data GameState = GameState {
                    players :: [P.Player],
                    cards :: [C.Card]
} deriving Show

-- TODO
-- drawFromDeck :: P.Player -> State GameState [Card]
-- drawFromDeck player = do
--     let deck = P.deck player
--     if (length deck) >= 5
--       then let newDeck = deck ++ P.discard player
--                newDiscard = []
--                draw = take 5 newDeck
--                newPlayer { P.deck = (drop 5 newDeck), P.discard = newDiscard }
--                savePlayer newPlayer
--                return draw
--       else let draw = take 5 deck
--                newPlayer { P.deck = (drop 5 deck) }
--                savePlayer newPlayer
--                return draw

drawFromDeck player = take 5 (P.deck player)

-- number of treasures this hand has
handValue :: [C.Card] -> Int
handValue hand = sum . catMaybes $ map coinValue hand

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x:xs) = Just x
firstJust (Nothing:xs) = firstJust xs

coinValue :: C.Card -> Maybe Int
coinValue card = firstJust $ map effect (C.effects card)
          where effect (C.CoinValue num) = Just num
                effect _ = Nothing

purchases :: P.Player -> C.Card -> State GameState ()
purchases player card = do
    state <- get
    let discards = P.discard player
        players_ = players state
        newPlayer = player { P.discard = card:discards }
        newPlayers = for players_ $ \player_ ->
                                        if (P.name player_) == (P.name newPlayer) then newPlayer else player_
    put $ state { players = newPlayers }
    return ()

playPlayer :: P.Player -> State GameState ()
playPlayer player = do
    let hand = drawFromDeck player
    purchaseCard (handValue hand)
  where
    purchaseCard money
      | money >= 6 = player `purchases` C.gold
      | money >= 3 = player `purchases` C.silver
      | otherwise  = player `purchases` C.copper

game :: State GameState ()
game = do
         state <- get
         let [player1, player2] = players state
         playPlayer player1
         playPlayer player2
