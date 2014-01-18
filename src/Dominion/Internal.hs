module Dominion.Internal where
import Prelude hiding (log)
import qualified Dominion.Types as T
import Dominion.Utils
import Text.Printf
import Control.Lens hiding (indices)
import Control.Monad.State hiding (state)
import Data.List
import Data.Ord
import qualified Dominion.Cards as CA
import Control.Arrow
import System.IO.Unsafe

coinValue :: T.Card -> Int
coinValue card = sum $ map effect (card ^. T.effects)
          where effect (T.CoinValue num) = num
                effect _ = 0

-- amount of money this player's hand is worth
handValue :: T.PlayerId -> T.Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (player ^. T.hand)) + (player ^. T.extraMoney)

log :: T.PlayerId -> String -> T.Dominion ()
log playerId str = do
    player <- getPlayer playerId
    money <- handValue playerId
    let name = player ^. T.playerName
        buys = player ^. T.buys
        actions = player ^. T.actions
        statusLine = printf "[player %s, name: %s, money: %s, buys: %s, actions: %s]" (yellow . show $ playerId) (yellow name) (green . show $ money) (green . show $ buys) (red . show $ actions)
    log_ $ statusLine ++ ": " ++ (green str)

log_ :: String -> T.Dominion ()
log_ str = do
    state <- get
    when (state ^. T.verbose) $ liftIO . putStrLn $ str

-- TODO check for 3 piles gone
gameOver cards
    | not (CA.province `elem` cards) = True
    | otherwise = False

-- given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
-- TODO if the deck doesn't have enough cards, we should draw the cards in
-- the deck before shuffling and drawing the rest.
drawFromDeck :: T.PlayerId -> Int -> T.Dominion ()
drawFromDeck playerId numCards = do
    player <- getPlayer playerId
    let deck = player ^. T.deck
    if (length deck) >= numCards
      then drawFromFull playerId numCards
      else do
        shuffleDeck playerId
        drawFromFull playerId numCards

-- takes a player id and a function.
-- That function takes a player and returns a modified player.
modifyPlayer :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyPlayer playerId func = modify $ \state -> over (T.players . element playerId) func $ state

-- player plays given strategy
playTurn :: T.PlayerId -> T.Strategy -> T.Dominion ()
playTurn playerId strategy = do
    drawFromDeck playerId 5
    modifyPlayer playerId $ \player -> set T.actions 1 $
                                       set T.buys 1 $
                                       set T.extraMoney 0 player
    player <- getPlayer playerId
    log playerId $ "player's hand has: " ++ (show . map T._name $ player ^. T.hand)
    strategy playerId
    discardHand playerId

game :: [T.Strategy] -> T.Dominion ()
game strategies = do
   state <- get
   forM_ (zip (indices $ state ^. T.players) strategies) (uncurry playTurn)

run :: T.GameState -> [T.Strategy] -> IO String
run state strategies = do
              (_, newState) <- runStateT (game strategies) state
              let cards = newState ^. T.cards
              if gameOver cards
                then returnResults newState
                else run (over T.round (+1) newState) strategies

countPoints :: T.Player -> Int
countPoints player = sum $ map countValue effects
    where cards        = player ^. T.deck ++ player ^. T.discard
          victoryCards = filter (\card -> T.Victory `elem` (card ^. T.cardType)) cards
          effects      = concatMap T._effects victoryCards
          countValue (T.VPValue x) = x
          countValue _ = 0

-- get player from game state
getPlayer :: T.PlayerId -> T.Dominion T.Player
getPlayer playerId = do
    state <- get
    return $ (state ^. T.players) !! playerId

returnResults :: T.GameState -> IO String
returnResults state = do
    let results = map (id &&& countPoints) (state ^. T.players)
    when (state ^. T.verbose) $ do
      putStrLn "Game Over!"
      forM_ results $ \(player, points) -> do
        putStrLn $ printf "player %s got %d points" (player ^. T.playerName) points
    return $ view (_1 . T.playerName) $ maximumBy (comparing snd) $ results

cardsOf count card = take count $ repeat card
pileOf card = 10 `cardsOf` card
 
eitherToBool :: (Either String ()) -> T.Dominion Bool
eitherToBool (Left _) = return False
eitherToBool (Right _) = return True

-- move this players discards + hand into his deck and shuffle the deck
shuffleDeck :: T.PlayerId -> T.Dominion ()
shuffleDeck playerId = modifyPlayer playerId shuffleDeck_

shuffleDeck_ :: T.Player -> T.Player
shuffleDeck_ player = set T.discard [] $ set T.deck newDeck player
          where discard = player ^. T.discard
                deck    = player ^. T.deck
                hand    = player ^. T.hand
                newDeck = unsafePerformIO $ deckShuffle (deck ++ discard ++ hand)

-- private method that gets called from `drawFromDeck`
-- only gets called when we know that the player has
-- at least 5 cards in his/her deck
drawFromFull :: T.PlayerId -> Int -> T.Dominion ()
drawFromFull playerId numCards = modifyPlayer playerId $ \player -> 
                            over T.deck (drop numCards) $ 
                              over T.hand (++ (take numCards (player ^. T.deck))) player

-- validate that this player is able to purchase this card
validateBuy :: T.PlayerId -> T.Card -> T.Dominion (Either String ())
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    if money < (card ^. T.cost)
      then return . Left $ printf "Not enough money. You have %d but this card costs %d" money (card ^. T.cost)
      else if not (card `elem` (state ^. T.cards))
             then return . Left $ printf "We've run out of that card (%s)" (card ^. T.name)
             else if (player ^. T.buys) < 1
                    then return . Left $ "You don't have any buys remaining!"
                    else return $ Right ()

-- check if a player can play a card
validatePlay :: T.PlayerId -> T.Card -> T.Dominion (Either String ())
validatePlay playerId card = do
    player <- getPlayer playerId
    if not (T.Action `elem` (card ^. T.cardType))
      then return . Left $ printf "%s is not an action card" (card ^. T.name)
      else if (player ^. T.actions) < 1
             then return . Left $ "You don't have any actions remaining!"
             else if not (card `elem` (player ^. T.hand))
                    then return . Left $ printf "You can't play a %s because you don't have it in your hand!" (card ^. T.name)
                    else return $ Right ()

discardHand :: T.PlayerId -> T.Dominion ()
discardHand playerId = modifyPlayer playerId $ \player -> set T.hand [] $ over T.discard (++ (player ^. T.hand)) player

findIteration :: [T.Option] -> Maybe Int
findIteration [] = Nothing
findIteration ((T.Iterations x):xs) = Just x
findIteration (_:xs) = findIteration xs

findLog :: [T.Option] -> Maybe Bool
findLog [] = Nothing
findLog ((T.Log x):xs) = Just x
findLog (_:xs) = findLog xs

-- used internally by the `plays` function.
-- Returns Nothing if the effect doesnt need anything else,
-- or returns (playerId, the effect) if its got a second
-- part (like with throne room or chapel).
usesEffect :: T.PlayerId -> T.CardEffect -> T.Dominion (Maybe (T.PlayerId, T.CardEffect))
playerId `usesEffect` (T.PlusAction x) = do
    log playerId ("+ " ++ (show x) ++ " actions")
    modifyPlayer playerId $ over T.actions (+x)
    return Nothing

playerId `usesEffect` (T.PlusCoin x) = do
    log playerId ("+ " ++ (show x) ++ " coin")
    modifyPlayer playerId $ over T.extraMoney (+x)
    return Nothing

playerId `usesEffect` (T.PlusBuy x) = do
    log playerId ("+ " ++ (show x) ++ " buys")
    modifyPlayer playerId $ over T.buys (+x)
    return Nothing

playerId `usesEffect` (T.PlusDraw x) = do
    log playerId ("+ " ++ (show x) ++ " cards")
    drawFromDeck playerId x
    return Nothing

playerId `usesEffect` effect@(T.PlayActionCard x) = do
    log playerId ("choose an action card and play it " ++ (show x) ++ " times")
    return $ Just (playerId, effect)
