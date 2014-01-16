{-# LANGUAGE TemplateHaskell #-}

module Dominion where
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

for = flip map
-- forM_ = flip mapM_

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


shuffleDeck playerId = modifyPlayer playerId shuffleDeck_

shuffleDeck_ player = set P.discard [] $ set P.deck newDeck player
          where discard = player ^. P.discard
                deck    = player ^. P.deck
                newDeck = unsafePerformIO $ myShuffle (deck ++ discard)

-- private method that gets called from `drawFromDeck`
-- only gets called when we know that the player has
-- at least 5 cards in his/her deck
drawFromFull playerId numCards = modifyPlayer playerId $ \player -> 
                            over P.deck (drop numCards) $ 
                              over P.hand (++ (take numCards (player ^. P.deck))) player
 
drawFromDeck :: PlayerId -> Int -> StateT GameState IO ()
drawFromDeck playerId numCards = do
    player <- getPlayer playerId
    let deck = player ^. P.deck
    if (length deck) >= numCards
      then drawFromFull playerId numCards
      else do
        shuffleDeck playerId
        drawFromFull playerId numCards

-- number of treasures this hand has
handValue :: PlayerId -> StateT GameState IO Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (player ^. P.hand)) + (player ^. P.extraMoney)

coinValue :: C.Card -> Int
coinValue card = sum $ map effect (card ^. C.effects)
          where effect (C.CoinValue num) = num
                effect _ = 0

-- player purchases a card
purchases :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
purchases playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    if money < (card ^. C.cost)
      then return . Left $ printf "Not enough money. You have %d but this card costs %d" money (card ^. C.cost)
      else if not (card `elem` (state ^. cards))
             then return . Left $ printf "We've run out of that card (%s)" (card ^. C.name)
             else if (player ^. P.buys) < 1
                    then return . Left $ "You don't have any buys remaining!"
                    else do
                       modifyPlayer playerId $ over P.discard (card:)
                       modify $ \state_ -> set cards (delete card (state_ ^. cards)) state_
                       liftIO $ putStrLn $ printf "player %d purchased a %s" playerId (card ^. C.name)
                       return $ Right ()

-- player plays an action card
plays :: PlayerId -> C.Card -> StateT GameState IO (Either String ())
plays playerId card = do
    player <- getPlayer playerId
    if not (C.Action `elem` (card ^. C.cardType))
      then return . Left $ printf "%s is not an action card" (card ^. C.name)
      else if (player ^. P.actions) < 1
             then return . Left $ "You don't have any actions remaining!"
             else do
               liftIO . putStrLn $ printf "player %d plays a %s!" playerId (card ^. C.name)
               mapM_ useEffect (card ^. C.effects)
               return $ Right ()
    where useEffect (C.PlusAction x) = modifyPlayer playerId $ over P.actions (+x)
          useEffect (C.PlusCoin x)   = modifyPlayer playerId $ over P.extraMoney (+x)
          useEffect (C.PlusBuy x)    = modifyPlayer playerId $ over P.buys (+x)
          useEffect (C.PlusDraw x)   = drawFromDeck playerId x

discardHand :: PlayerId -> StateT GameState IO ()
discardHand playerId = modifyPlayer playerId $ \player -> set P.hand [] $ over P.discard (++ (player ^. P.hand)) player

-- the big money strategy
bigMoney playerId = do
    money <- handValue playerId
    bigMoney_ playerId money

bigMoney_ playerId money
    | money >= 8 = playerId `purchases` C.province
    | money >= 6 = playerId `purchases` C.gold
    | money >= 5 = playerId `purchases` C.duchy
    | money >= 3 = playerId `purchases` C.silver
    | otherwise  = playerId `purchases` C.copper

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

-- see if a player has a card in his hand
has :: P.Player -> C.Card -> Bool
has player card = card `elem` (player ^. P.hand)

bigMoneySmithy playerId = do
    player <- getPlayer playerId
    when (player `has` C.smithy) $ playerId `plays` C.smithy >> return ()
    money <- handValue playerId
    bigMoneySmithy_ playerId money player

bigMoneySmithy_ playerId money player
    | money >= 8 = playerId `purchases` C.province
    | money >= 6 = playerId `purchases` C.gold
    | money >= 5 = playerId `purchases` C.duchy
    | money >= 4 && (count C.smithy (P.allCards player) < 2) = playerId `purchases` C.smithy
    | money >= 3 = playerId `purchases` C.silver
    | otherwise  = playerId `purchases` C.copper


-- player plays given strategy
playTurn playerId strategy = do
    drawFromDeck playerId 5
    modifyPlayer playerId $ \player -> set P.actions 1 $ set P.buys 1 $ set P.extraMoney 0 player
    strategy playerId
    discardHand playerId

game :: StateT GameState IO ()
game = do
         state <- get
         forM_ (zip (state ^. players) [0..]) $ \(p, p_id) -> if (p ^. P.name == "maggie")
                                                                then playTurn p_id bigMoney
                                                                else playTurn p_id bigMoneySmithy
