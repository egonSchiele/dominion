{-# LANGUAGE TemplateHaskell #-}

module Dominion where
import Prelude hiding (log)
import qualified Player as P
import qualified Card as C
import Control.Monad
import Data.Maybe
import Control.Monad.State hiding (state)
import Control.Concurrent
import Control.Lens hiding (has)
import Control.Monad.IO.Class
import Text.Printf
import Data.List
import Data.Random.Extras
import Data.Random hiding (shuffle)
import System.Random
import System.IO.Unsafe
import Control.Monad
import qualified Debug.Trace as D
import Utils

eitherToBool :: (Either String ()) -> StateT GameState IO Bool
eitherToBool (Left _) = return False
eitherToBool (Right _) = return True

coinValue :: C.Card -> Int
coinValue card = sum $ map effect (card ^. C.effects)
          where effect (C.CoinValue num) = num
                effect _ = 0

myShuffle :: [C.Card] -> IO [C.Card]
myShuffle deck = do
    gen <- getStdGen
    let (shuffled, newGen) = sampleState (shuffle deck) gen
    setStdGen newGen
    return shuffled

type PlayerId = Int

data GameState = GameState {
                    _players :: [P.Player],
                    _cards :: [C.Card]
} deriving Show

makeLenses ''GameState

-- get player from game state
getPlayer :: PlayerId -> StateT GameState IO P.Player
getPlayer playerId = do
    state <- get
    return $ (state ^. players) !! playerId

-- takes a player id and a function.
-- That function takes a player and returns a modified player.
modifyPlayer :: PlayerId -> (P.Player -> P.Player) -> StateT GameState IO ()
modifyPlayer playerId func = modify $ \state -> over (players . element playerId) func $ state

-- move this players discards + hand into his deck and shuffle the deck
shuffleDeck playerId = modifyPlayer playerId shuffleDeck_

shuffleDeck_ player = set P.discard [] $ set P.deck newDeck player
          where discard = player ^. P.discard
                deck    = player ^. P.deck
                hand    = player ^. P.hand
                newDeck = unsafePerformIO $ myShuffle (deck ++ discard ++ hand)

-- private method that gets called from `drawFromDeck`
-- only gets called when we know that the player has
-- at least 5 cards in his/her deck
drawFromFull playerId numCards = modifyPlayer playerId $ \player -> 
                            over P.deck (drop numCards) $ 
                              over P.hand (++ (take numCards (player ^. P.deck))) player
 
-- given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
-- TODO if the deck doesn't have enough cards, we should draw the cards in
-- the deck before shuffling and drawing the rest.
drawFromDeck :: PlayerId -> Int -> StateT GameState IO ()
drawFromDeck playerId numCards = do
    player <- getPlayer playerId
    let deck = player ^. P.deck
    if (length deck) >= numCards
      then drawFromFull playerId numCards
      else do
        shuffleDeck playerId
        drawFromFull playerId numCards

-- amount of money this player's hand is worth
handValue :: PlayerId -> StateT GameState IO Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (player ^. P.hand)) + (player ^. P.extraMoney)

-- validate that this player is able to purchase this card
validateBuy :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    if money < (card ^. C.cost)
      then return . Left $ printf "Not enough money. You have %d but this card costs %d" money (card ^. C.cost)
      else if not (card `elem` (state ^. cards))
             then return . Left $ printf "We've run out of that card (%s)" (card ^. C.name)
             else if (player ^. P.buys) < 1
                    then return . Left $ "You don't have any buys remaining!"
                    else return $ Right ()

-- player buys a card
buys :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
buys playerId card = do
    state <- get
    validation <- validateBuy playerId card
    case validation of
      Left x -> return $ Left x
      Right _ -> do
                   modifyPlayer playerId $ \player -> over P.discard (card:) $ over P.buys (subtract 1) $ over P.extraMoney (subtract $ card ^. C.cost) player
                   modify $ \state_ -> set cards (delete card (state_ ^. cards)) state_
                   -- liftIO $ putStrLn $ printf "player %d bought a %s" playerId (card ^. C.name)
                   return $ Right ()

-- Give an array of cards, in order of preference of buy.
-- We'll try to buy as many cards as possible, in order of preference.
buysByPreference :: PlayerId -> [C.Card] -> StateT GameState IO ()
buysByPreference playerId cards = do
    player <- getPlayer playerId
    when ((player ^. P.buys) > 0) $ do
      purchasableCards <- filterM (\card -> validateBuy playerId card >>= eitherToBool) cards
      when (not (null purchasableCards)) $ do
        playerId `buys` (head purchasableCards)
        playerId `buysByPreference` cards

-- Give an array of cards, in order of preference of play.
-- We'll try to play as many cards as possible, in order of preference.
playsByPreference :: PlayerId -> [C.Card] -> StateT GameState IO ()
playsByPreference playerId cards = do
    player <- getPlayer playerId
    when ((player ^. P.actions) > 0) $ do
      playableCards <- filterM (\card -> validatePlay playerId card >>= eitherToBool) cards
      when (not (null playableCards)) $ do
        playerId `plays` (head playableCards)
        playerId `playsByPreference` cards

log str x = x

-- check if a player can play a card
validatePlay :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
validatePlay playerId card = do
    player <- getPlayer playerId
    if not (C.Action `elem` (card ^. C.cardType))
      then return . Left $ printf "%s is not an action card" (card ^. C.name)
      else if (player ^. P.actions) < 1
             then return . Left $ "You don't have any actions remaining!"
             else if not (card `elem` (player ^. P.hand))
                    then return . Left $ printf "You can't play a %s because you don't have it in your hand!" (card ^. C.name)
                    else return $ Right ()

-- player plays an action card
plays :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
plays playerId card = do
    validation <- validatePlay playerId card
    case validation of
      Left x -> return $ Left x
      Right _ -> do
               -- liftIO . putStrLn $ printf "player %d plays a %s!" playerId (card ^. C.name)
               mapM_ useEffect (card ^. C.effects)
               modifyPlayer playerId $ \player -> over P.hand (delete card) $ over P.discard (card:) $ over P.actions (subtract 1) $ player
               return $ Right ()
    where useEffect (C.PlusAction x) = log ("+ " ++ (show x) ++ " actions") modifyPlayer playerId $ over P.actions (+x)
          useEffect (C.PlusCoin x)   = log ("+ " ++ (show x) ++ " coin") modifyPlayer playerId $ over P.extraMoney (+x)
          useEffect (C.PlusBuy x)    = log ("+ " ++ (show x) ++ " buys") modifyPlayer playerId $ over P.buys (+x)
          useEffect (C.PlusDraw x)   = log ("+ " ++ (show x) ++ " cards") drawFromDeck playerId x

discardHand :: PlayerId -> StateT GameState IO ()
discardHand playerId = modifyPlayer playerId $ \player -> set P.hand [] $ over P.discard (++ (player ^. P.hand)) player

-- see if a player has a card in his hand
has :: P.Player -> C.Card -> Bool
has player card = card `elem` (player ^. P.hand)

-- player plays given strategy
playTurn playerId strategy = do
    drawFromDeck playerId 5
    modifyPlayer playerId $ \player -> set P.actions 1 $ set P.buys 1 $ set P.extraMoney 0 player
    strategy playerId
    discardHand playerId
