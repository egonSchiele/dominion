{-# LANGUAGE TemplateHaskell #-}

module Dominion where
import qualified Player as P
import qualified Card as C
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Text.Printf

for = flip map

type PlayerId = Int

data GameState = GameState {
                    _players :: [P.Player],
                    _cards :: [C.Card]
} deriving Show

makeLenses ''GameState

getPlayer :: PlayerId -> StateT GameState IO P.Player
getPlayer playerId = do
    state <- get
    return $ (state ^. players) !! playerId

savePlayer :: P.Player -> PlayerId -> StateT GameState IO ()
savePlayer player playerId = do
    state <- get
    -- WOO lenses!
    let newState = set (players . element playerId) player $ state
    put newState

drawFromDeck :: PlayerId -> StateT GameState IO [C.Card]
drawFromDeck playerId = do
    player <- getPlayer playerId
    let deck = player ^. P.deck
    if (length deck) >= 5
      then drawFromFull
      else shuffleDeck >> drawFromFull

  where shuffleDeck = do
          player <- getPlayer playerId
          let discard = player ^. P.discard
              -- TODO not sure of this syntax
              newPlayer = set P.discard [] $ over P.deck (++ discard) player
          savePlayer newPlayer playerId

        drawFromFull = do
          player <- getPlayer playerId
          let deck = player ^. P.deck
              draw = take 5 deck
              newPlayer = over P.deck (drop 5) player
          savePlayer newPlayer playerId
          return draw

-- drawFromDeck player = take 5 (P._deck player)

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

purchases :: PlayerId -> C.Card -> StateT GameState IO ()
purchases playerId card = do
    player <- getPlayer playerId
    let newPlayer = over P.discard (card:) player
    savePlayer newPlayer playerId
    liftIO $ putStrLn $ printf "player %d purchased a %s" playerId (C.name card)
    return ()

discardHand :: PlayerId -> [C.Card] -> StateT GameState IO ()
discardHand playerId hand = do
    player <- getPlayer playerId
    let newPlayer = over P.discard (++hand) player
    savePlayer newPlayer playerId
    return ()

playPlayer :: PlayerId -> StateT GameState IO ()
playPlayer playerId = do
    hand <- drawFromDeck playerId
    liftIO $ print (map C.name hand)
    purchaseCard (handValue hand)
    discardHand playerId hand
  where
    purchaseCard money
      | money >= 6 = playerId `purchases` C.gold
      | money >= 3 = playerId `purchases` C.silver
      | otherwise  = playerId `purchases` C.copper

game :: StateT GameState IO ()
game = do
         state <- get
         mapM_ (playPlayer . snd) (zip (state ^. players) [0..])
