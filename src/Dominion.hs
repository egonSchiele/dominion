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
import Data.List
import Data.Random.Extras
import Data.Random hiding (shuffle)
import System.Random

for = flip map
forM_ = flip mapM_

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

myShuffle deck = do
    gen <- getStdGen
    let (shuffled, newGen) = sampleState (shuffle deck) gen
    setStdGen newGen
    return shuffled

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
              deck    = player ^. P.deck
          newDeck <- liftIO $ myShuffle (deck ++ discard)
          let newPlayer = set P.discard [] $ set P.deck newDeck player
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
    state <- get
    let newPlayer = over P.discard (card:) player
        newCards  = delete card (state ^. cards)
        newState  = set cards newCards state
    put newState
    savePlayer newPlayer playerId
    liftIO $ putStrLn $ printf "player %d purchased a %s" playerId (C.name card)
    return ()

discardsHand :: PlayerId -> [C.Card] -> StateT GameState IO ()
discardsHand playerId hand = do
    player <- getPlayer playerId
    let newPlayer = over P.discard (++hand) player
    savePlayer newPlayer playerId
    return ()

bigMoney playerId hand
    | (handValue hand) >= 8 = playerId `purchases` C.province
    | (handValue hand) >= 6 = playerId `purchases` C.gold
    | (handValue hand) >= 5 = playerId `purchases` C.duchy
    | (handValue hand) >= 3 = playerId `purchases` C.silver
    | otherwise  = playerId `purchases` C.copper

-- plays :: PlayerId -> StateT GameState IO ()
plays playerId strategy = do
    hand <- drawFromDeck playerId
    liftIO $ print (map C.name hand)
    strategy playerId hand
    playerId `discardsHand` hand

game :: StateT GameState IO ()
game = do
         state <- get
         forM_ (zip (state ^. players) [0..]) $ \(_, p_id) -> p_id `plays` bigMoney
