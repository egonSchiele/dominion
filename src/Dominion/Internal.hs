module Dominion.Internal (

  -- | Note: You shouldn't need to import this module...the
  -- interesting functions are re-exported by the Dominion module.
  --
  -- Use any other functions in here at your own risk.
  module Dominion.Internal
) where
import           Control.Applicative
import           Control.Arrow
import           Control.Lens        hiding (has, indices)
import           Control.Monad.State hiding (state)
import           Data.List
import           Data.Ord
import qualified Dominion.Cards      as CA
import qualified Dominion.Types      as T
import           Dominion.Utils
import           Prelude             hiding (log)
import           System.IO.Unsafe
import           Text.Printf
import Control.Monad.Error

-- | see if a player has a card in his hand.
--
-- > hasCard <- playerId `has` chapel
has :: T.PlayerId -> T.Card -> T.Dominion Bool
has playerId card = do
    player <- getPlayer playerId
    return $ card `elem` (player ^. T.hand)

-- | see how many of this card a player has.
--
-- > numMarkets <- countNum playerId market
countNum :: T.PlayerId -> T.Card -> T.Dominion Int
countNum playerId card = do
    player <- getPlayer playerId
    let allCards = player ^. T.deck ++ player ^. T.discard ++ player ^. T.hand
    return $ count card allCards

-- | What this card is worth in money.
coinValue :: T.Card -> Int
coinValue card = sum $ map effect (card ^. T.effects)
          where effect (T.CoinValue num) = num
                effect _ = 0

-- | Get the current round number.
getRound :: T.Dominion Int
getRound = T._round <$> get

-- | How much money this player's hand is worth (also counts any money you
-- get from action cards, like +1 from market).
handValue :: T.PlayerId -> T.Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (player ^. T.hand)) + (player ^. T.extraMoney)

-- | Check if this card's pile is empty.
pileEmpty :: T.Card -> T.Dominion Bool
pileEmpty card = do
    state <- get
    return $ card `elem` (state ^. T.cards)

-- | Returns the card, or Nothing if that pile is empty.
-- Useful because it automatically checks whether the pile is empty, and
-- modifies state to subtract a card from the pile correctly.
getCard :: T.Card -> T.Dominion (Maybe T.Card)
getCard card = do
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        modify $ over T.cards (delete card)
        return $ Just card

-- | Convenience function. Prints out a line if verbose, AND prints out
-- info about the related player...name, money, # of buys, # of actions.
log :: T.PlayerId -> String -> T.Dominion ()
log playerId str = do
    player <- getPlayer playerId
    money <- handValue playerId
    let name = player ^. T.playerName
        buys = player ^. T.buys
        actions = player ^. T.actions
        statusLine = printf "[player %s, name: %s, money: %s, buys: %s, actions: %s]" (yellow . show $ playerId) (yellow name) (green . show $ money) (green . show $ buys) (red . show $ actions)
    log_ $ statusLine ++ ": " ++ (green str)

-- | Like `log` but doesn't print out info about a player
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

-- | Given a player id and a number of cards to draw, draws that many cards
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

-- | Like `modify` for the `State` monad, but works on players.
-- Takes a player id and a function that modifies the player.
modifyPlayer :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyPlayer playerId func = modify $ over (T.players . element playerId) func

-- | Like `modifyPlayer`, but modifies every player *except* the one specified with the player id.
modifyOtherPlayers :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyOtherPlayers playerId func = do
    state <- get
    let players = (indices (state ^. T.players)) \\ [playerId]
    forM_ players $ \pid -> modify $ over (T.players . element pid) func

setupForTurn :: T.PlayerId -> T.Dominion ()
setupForTurn playerId = do
    drawFromDeck playerId 5
    modifyPlayer playerId $ set T.actions 1 . set T.buys 1 . set T.extraMoney 0

playTurn :: T.PlayerId -> T.Strategy -> T.Dominion ()
playTurn playerId strategy = do
    roundNum <- getRound
    when (roundNum == 1) $ setupForTurn playerId
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
   forM_ (zip ids strategies) (uncurry playTurn)

run :: T.GameState -> [T.Strategy] -> IO T.Result
run state strategies = do
              (_, newState) <- runStateT (runErrorT (game strategies)) state
              let cards = newState ^. T.cards
              if gameOver cards
                then returnResults newState
                else run (over T.round (+1) newState) strategies

returnResults :: T.GameState -> IO T.Result
returnResults state = do
    let results = map (id &&& countPoints) (state ^. T.players)
        winner  = view (_1 . T.playerName) $ maximumBy (comparing snd) $ results
    when (state ^. T.verbose) $ do
      putStrLn "Game Over!"
      forM_ results $ \(player, points) -> do
        putStrLn $ printf "player %s got %d points" (player ^. T.playerName) points
    return $ T.Result results winner

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

-- | Get player from game state specified by this id.
-- This is useful sometimes:
--
-- > import qualified Dominion.Types as T
-- > import Control.Lens
-- >
-- > player <- getPlayer playerId
-- >
-- > -- How many buys does this player have?
-- > player ^. T.buys
-- >
-- > -- How many actions does this player have?
-- > player ^. T.actions
getPlayer :: T.PlayerId -> T.Dominion T.Player
getPlayer playerId = do
    state <- get
    return $ (state ^. T.players) !! playerId

-- | Convenience function. @ 4 \`cardsOf\` estate @ is the same as @ take 4 . repeat $ estate @
cardsOf count card = take count $ repeat card

pileOf card
  | card == CA.copper   = 60 `cardsOf` CA.copper
  | card == CA.silver   = 40 `cardsOf` CA.silver
  | card == CA.gold     = 30 `cardsOf` CA.gold
  | card == CA.estate   = 12 `cardsOf` CA.estate
  | card == CA.duchy    = 12 `cardsOf` CA.duchy
  | card == CA.province = 12 `cardsOf` CA.province
  | otherwise           = 10 `cardsOf` card

eitherToBool :: (Either String ()) -> Bool
eitherToBool (Left _) = False
eitherToBool (Right _) = True

-- | Move this players discards + hand into his deck and shuffle the deck.
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


failIf :: Bool -> String -> T.Dominion Bool
failIf True str = throwError str >> return False
failIf False str = return True

-- | Check that this player is able to purchase this card. Returns
-- a `Right` if they can purchase the card, otherwise returns a `Left` with
-- the reason why they can't purchase it.
validateBuy :: T.PlayerId -> T.Card -> T.Dominion Bool
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    failIf (money < (card ^. T.cost)) $ printf "Not enough money. You have %d but this card costs %d" money (card ^. T.cost)
    failIf (not (card `elem` (state ^. T.cards))) $ printf "We've run out of that card (%s)" (card ^. T.name)
    failIf ((player ^. T.buys) < 1) $ "You don't have any buys remaining!"

-- | Check that this player is able to play this card. Returns
-- a `Right` if they can play the card, otherwise returns a `Left` with
-- the reason why they can't play it.
validatePlay :: T.PlayerId -> T.Card -> T.Dominion Bool
validatePlay playerId card = do
    player <- getPlayer playerId
    failIf (not (isAction card)) $ printf "%s is not an action card" (card ^. T.name)
    failIf ((player ^. T.actions) < 1) $ "You don't have any actions remaining!"
    failIf (not (card `elem` (player ^. T.hand))) $ printf "You can't play a %s because you don't have it in your hand!" (card ^. T.name)

-- Discard this player's hand.
discardHand :: T.PlayerId -> T.Dominion ()
discardHand playerId = modifyPlayer playerId $ \player -> set T.hand [] $ over T.discard (++ (player ^. T.hand)) player

-- for parsing options
findIteration :: [T.Option] -> Maybe Int
findIteration [] = Nothing
findIteration ((T.Iterations x):xs) = Just x
findIteration (_:xs) = findIteration xs

-- for parsing options
findLog :: [T.Option] -> Maybe Bool
findLog [] = Nothing
findLog ((T.Log x):xs) = Just x
findLog (_:xs) = findLog xs

-- for parsing options
findCards :: [T.Option] -> Maybe [T.Card]
findCards [] = Nothing
findCards ((T.Cards x):xs) = Just x
findCards (_:xs) = findCards xs

-- | Keep drawing a card until the provided function returns true.
-- The function gets a list of the cards drawn so far,
-- most recent first. Returns a list of all the cards drawn (these cards
-- are also placed into the player's hand)
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

-- Does this card say you trash it when you play it?
trashThisCard :: T.Card -> Bool
trashThisCard card = T.TrashThisCard `elem` (card ^. T.effects)

-- | Player trashes the given card.
trashesCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `trashesCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ do
    modifyPlayer playerId (over T.hand (delete card))

-- | Player discards the given card.
discardsCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `discardsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ do
    modifyPlayer playerId $ over T.hand (delete card) . over T.discard (card:)

-- Player returns the given card to the top of their deck.
returnsCard :: T.PlayerId -> T.Card -> T.Dominion ()
playerId `returnsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ do
    modifyPlayer playerId $ over T.hand (delete card) . over T.deck (card:)

-- If the top card in the player's deck is one of the cards
-- listed in the provided array, then discard that card (used with spy).
discardTopCard :: [T.Card] -> T.Player -> T.Player
discardTopCard cards player = if (topCard `elem` cards)
                                then set T.deck (tail deck) . over T.discard (topCard:) $ player
                                else player
    where topCard = head $ player ^. T.deck
          deck = player ^. T.deck

-- If this player has a victory card in his/her hand,
-- it is put on top of their deck *unless* they have a moat in their hand.
-- Used with militia.
returnVPCard :: T.Player -> T.Player
returnVPCard player = let hand = player ^. T.hand
                          victoryCards = filter isVictory hand
                          card = head victoryCards
                      in if (CA.moat `elem` hand || null victoryCards)
                          then player
                          else over T.hand (delete card) $ over T.deck (card:) player

-- TODO how do they choose what to discard??
-- Right now I'm just choosing to discard the least expensive.
-- | Player discards down to x cards.
discardsTo :: T.Player -> Int -> T.Player
player `discardsTo` x = set T.hand toKeep . over T.discard (++ toDiscard) $ player
    where hand = sortBy (comparing T._cost) $ player ^. T.hand
          toDiscard = take (length hand - x) hand
          toKeep = hand \\ toDiscard

-- | Used internally by the `plays` function. Each card has a list of
-- effects (like smithy has `PlusCard 3`). This function applies the given
-- effect. It returns `Nothing` if the effect doesn't need a `Followup`,
-- or it returns a `Just Followup`.
usesEffect :: T.PlayerId -> T.CardEffect -> T.Dominion (Maybe T.Followup)
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
playerId `usesEffect` _ = return Nothing

-- | Given a name, creates a player with that name.
makePlayer :: String -> T.Player
makePlayer name = T.Player name [] (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate) [] 1 1 0

-- Checks that the player can gain the given card, then adds it to his/her
-- discard pile.
gainCardUpTo :: T.PlayerId -> Int -> T.Card -> T.Dominion (Maybe [T.Followup])
gainCardUpTo playerId value card = do
  let errString = printf "Card is too expensive. You can gain a card costing up to %d but this card costs %d" value (card ^. T.cost)
  when ((card ^. T.cost) > value) $ throwError errString
  result <- getCard card
  case result of
    Nothing -> throwError $ printf "We've run out of that card (%s)" (card ^. T.name)
    (Just card_) -> do
      modifyPlayer playerId $ over T.discard (card_:)
      return Nothing
