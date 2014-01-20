module Dominion.Internal where
import Prelude hiding (log)
import qualified Dominion.Types as T
import Dominion.Utils
import Text.Printf
import Control.Lens hiding (indices, has)
import Control.Monad.State hiding (state)
import Data.List
import Data.Ord
import qualified Dominion.Cards as CA
import Control.Arrow
import System.IO.Unsafe

-- see if a player has a card in his hand
has :: T.PlayerId -> T.Card -> T.Dominion Bool
has playerId card = do
    player <- getPlayer playerId
    return $ card `elem` (player ^. T.hand)

coinValue :: T.Card -> Int
coinValue card = sum $ map effect (card ^. T.effects)
          where effect (T.CoinValue num) = num
                effect _ = 0

-- amount of money this player's hand is worth
handValue :: T.PlayerId -> T.Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (player ^. T.hand)) + (player ^. T.extraMoney)

-- check if you are out of a particular card
pileEmpty :: T.Card -> T.Dominion Bool
pileEmpty card = do
    state <- get
    return $ card `elem` (state ^. T.cards)

-- get a card. Returns the gained card, or Nothing if that pile is empty.
getCard :: T.Card -> T.Dominion (Maybe T.Card)
getCard card = do
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        modify $ over T.cards (delete card)
        return $ Just card

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

gameOver cards
    | not (CA.province `elem` cards) = True
    -- (copper, silver, gold) + (curse, estate, duchy, province) + 10
    -- action cards minus 3. Any three piles gone = game over.
    | length (nub cards) <= (3 + 3 + 10 - 3) = True
    | otherwise = False

-- given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
-- TODO if the deck doesn't have enough cards, we should draw the cards in
-- the deck before shuffling and drawing the rest.
drawFromDeck :: T.PlayerId -> Int -> T.Dominion [T.Card]
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
modifyPlayer playerId func = modify $ over (T.players . element playerId) func

-- modifies every player *except* the one specified with the player id
modifyOtherPlayers :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyOtherPlayers playerId func = do
    state <- get
    let players = (indices (state ^. T.players)) \\ [playerId]
    forM_ players $ \pid -> modify $ over (T.players . element pid) func

setupForTurn playerId = do
    drawFromDeck playerId 5
    modifyPlayer playerId $ set T.actions 1 . set T.buys 1 . set T.extraMoney 0

-- player plays given strategy
playTurn :: T.PlayerId -> T.Strategy -> T.Dominion ()
playTurn playerId strategy = do
    player <- getPlayer playerId
    log playerId $ "player's hand has: " ++ (show . map T._name $ player ^. T.hand)
    strategy playerId
    discardHand playerId
    -- we draw from deck *after* to set up the next hand NOW,
    -- instead of calling this at the beginning of the function.
    -- The reason is, if someone else plays a militia, or a council room,
    -- these players need to be able to modify their deck accordingly
    -- even if its not their turn.
    setupForTurn playerId

game :: [T.Strategy] -> T.Dominion ()
game strategies = do
   state <- get
   let ids = indices $ state ^. T.players
   forM_ ids setupForTurn
   forM_ (zip ids strategies) (uncurry playTurn)

run :: T.GameState -> [T.Strategy] -> IO String
run state strategies = do
              (_, newState) <- runStateT (game strategies) state
              let cards = newState ^. T.cards
              if gameOver cards
                then returnResults newState
                else run (over T.round (+1) newState) strategies

isAction card = T.Action `elem` (card ^. T.cardType)
isAttack card = T.Attack `elem` (card ^. T.cardType)
isReaction card = T.Reaction `elem` (card ^. T.cardType)
isTreasure card = T.Treasure `elem` (card ^. T.cardType)
isVictory card = T.Victory `elem` (card ^. T.cardType)

countPoints :: T.Player -> Int
countPoints player = sum $ map countValue effects
    where cards        = player ^. T.deck ++ player ^. T.discard ++ player ^. T.hand
          victoryCards = filter isVictory cards
          effects      = concatMap T._effects victoryCards
          countValue (T.VPValue x) = x
          countValue (T.GardensEffect) = length cards `div` 10
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
 
eitherToBool :: (Either String ()) -> Bool
eitherToBool (Left _) = False
eitherToBool (Right _) = True

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
-- returns the drawn cards and adds them to the player's hand
drawFromFull :: T.PlayerId -> Int -> T.Dominion [T.Card]
drawFromFull playerId numCards = do
    player <- getPlayer playerId
    let drawnCards = (take numCards (player ^. T.deck))
    modifyPlayer playerId $ over T.deck (drop numCards) . over T.hand (++ drawnCards)
    return drawnCards

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
    if not (isAction card)
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

-- keep drawing a card until the provided function returns true.
-- the function gets a list of the cards drawn so far,
-- most recent first. Returns a list of all the cards drawn (these cards
-- are also in the player's hand)
drawsUntil :: T.PlayerId -> ([T.Card] -> T.Dominion Bool) -> T.Dominion [T.Card]
drawsUntil = drawsUntil_ []

-- internal use for drawsUntil
drawsUntil_ :: [T.Card] -> T.PlayerId -> ([T.Card] -> T.Dominion Bool) -> T.Dominion [T.Card]
drawsUntil_ alreadyDrawn playerId func = do
    drawnCards <- drawFromDeck playerId 1
    let cards = drawnCards ++ alreadyDrawn
    stopDrawing <- func cards
    if stopDrawing
      then return cards
      else drawsUntil_ cards playerId func

trashThisCard :: T.Card -> Bool
trashThisCard card = T.TrashThisCard `elem` (card ^. T.effects)

trashesCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `trashesCard` card = modifyPlayer playerId (over T.hand (delete card))

discardsCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `discardsCard` card = modifyPlayer playerId $ over T.hand (delete card) . over T.discard (card:)

-- return to the top of their deck
returnsCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `returnsCard` card = modifyPlayer playerId $ over T.hand (delete card) . over T.deck (card:)

-- if the top card in the player's deck is one of the cards
-- listed in the provided array, then discard that card (used with spy)
discardTopCard :: [T.Card] -> T.Player -> T.Player
discardTopCard cards player = if (topCard `elem` cards)
                                then set T.deck (tail deck) . over T.discard (topCard:) $ player
                                else player
    where topCard = head $ player ^. T.deck
          deck = player ^. T.deck

-- if this player has a victory card in his/her hand,
-- it is put on top of their deck *unless* they have a moat in their hand
returnVPCard :: T.Player -> T.Player
returnVPCard player = let hand = player ^. T.hand
                          victoryCards = filter isVictory hand
                          card = head victoryCards
                      in if (CA.moat `elem` hand || null victoryCards)
                          then player
                          else over T.hand (delete card) $ over T.deck (card:) player

-- TODO how do they choose what to discard??
-- Right now I'm just choosing to discard the least expensive
discardsTo :: T.Player -> Int -> T.Player
player `discardsTo` x = set T.hand toKeep . over T.discard (++ toDiscard) $ player
    where hand = sortBy (comparing T._cost) $ player ^. T.hand
          toDiscard = take (length hand - x) hand
          toKeep = hand \\ toDiscard

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

playerId `usesEffect` (T.PlusCard x) = do
    log playerId ("+ " ++ (show x) ++ " cards")
    drawFromDeck playerId x
    return Nothing

playerId `usesEffect` effect@(T.PlayActionCard x) = do
    return $ Just (playerId, effect)

playerId `usesEffect` (T.AdventurerEffect) = do
    log playerId "finding the next two treasures from your deck..."
    drawnCards <- playerId `drawsUntil` (\cards -> return $ (countBy isTreasure cards) == 2)
    -- the cards that weren't treasures need to be discarded
    forM_ (filter (not . isTreasure) drawnCards) $ \card -> playerId `discardsCard` card
    return Nothing

playerId `usesEffect` (T.BureaucratEffect) = do
    card_ <- getCard CA.silver
    case card_ of
      Nothing -> return ()
      Just card -> do
        log playerId "+ silver"
        modifyPlayer playerId $ over T.deck (card:)
    modifyOtherPlayers playerId returnVPCard
    return Nothing

playerId `usesEffect` effect@(T.CellarEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.ChancellorEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.TrashCards x) = do
    log playerId ("Trash up to " ++ (show x) ++ " cards from your hand.")
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.OthersPlusCard x) = do
    log playerId ("Every other player draws " ++ (show x) ++ " card.")
    state <- get
    let players = (indices (state ^. T.players)) \\ [playerId]
    forM_ players $ \pid -> drawFromDeck pid 1
    return Nothing

playerId `usesEffect` effect@(T.GainCardUpto x) = do
    log playerId ("Gain a card costing up to " ++ (show x) ++ " coins.")
    return $ Just (playerId, effect)

-- TODO this doesn't set aside any action cards.
-- How do I implement the logic for choosing that?
-- Basically it allows the player to go through
-- and choose the action card they want?
playerId `usesEffect` effect@(T.LibraryEffect) = do
    log playerId ("Drawing to 7 cards...")
    drawsUntil playerId $ \_ -> do
                 player <- getPlayer playerId
                 return $ length (player ^. T.hand) == 7
    return Nothing

-- NOTE: one side effect of this + council room is:
-- every player needs to draw their next hand immediately
-- after they finish playing, instead of at the start of when
-- they play. Otherwise suppose someone plays a council room
-- followed by a militia. I need to codify that properly.
playerId `usesEffect` effect@(T.OthersDiscardTo x) = do
    log playerId ("Every other player discards down to " ++ (show x) ++ " cards.")
    modifyOtherPlayers playerId (\p -> p `discardsTo` x)
    return Nothing

playerId `usesEffect` effect@(T.MineEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.MoneylenderEffect) = do
    hasCard <- playerId `has` CA.copper
    when hasCard $ do
      log playerId ("Trashing a copper. +3 coin")
      playerId `trashesCard` CA.copper
      modifyPlayer playerId $ over (T.extraMoney) (+3)
    return Nothing

playerId `usesEffect` effect@(T.RemodelEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.SpyEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.ThiefEffect) = do
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.OthersGainCurse x) = do
    log playerId ("All other players gain " ++ (show x) ++ " curses.")
    let card = CA.curse
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        modifyOtherPlayers playerId (over T.discard (card:))
        state <- get
        times (length (state ^. T.players) - 1) $ do
          modify $ over T.cards (delete card)
          return ()
        return Nothing

-- only counted at the end of the game.
playerId `usesEffect` effect@(T.GardensEffect) = return Nothing
