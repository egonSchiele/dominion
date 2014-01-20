{-# LANGUAGE TemplateHaskell #-}

module Dominion (Option(..), module Dominion) where
import Prelude hiding (log)
import qualified Dominion.Types as T
import Dominion.Types (Option(..))
import qualified Dominion.Cards as CA
import Control.Monad hiding (join)
import Data.Maybe
import Control.Monad.State hiding (state, join)
import Control.Lens hiding (indices, has)
import Control.Monad.IO.Class
import Text.Printf
import Data.List
import Dominion.Utils
import Data.Either
import Control.Applicative
import Dominion.Internal

makePlayer :: String -> T.Player
makePlayer name = T.Player name [] (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate) [] 1 1 0

uses :: String -> T.Strategy -> (T.Player, T.Strategy)
name `uses` strategy = ((makePlayer name), strategy)

dominion :: [(T.Player, T.Strategy)] -> IO ()
dominion = dominionWithOpts []

dominionWithOpts :: [T.Option] -> [(T.Player, T.Strategy)] -> IO ()
dominionWithOpts options list = do
    actionCards <- deckShuffle CA.allCards
    let players = map fst list
        strategies = map snd list
        iterations = findIteration options |||| 1000
        verbose_ = findLog options |||| False
        cards = concatMap pileOf $ CA.treasureCards ++ CA.victoryCards ++ (take 10 actionCards)
    when verbose_ $ putStrLn $ "Playing with: " ++ (join ", " . map T._name $ actionCards)

    -- TODO we should cycle through all players to give each one an even
    -- chance at going first
    results <- forM [1..iterations] $ \i -> if even i
                                        then run (T.GameState players cards 1 verbose_) strategies
                                        else run (T.GameState (reverse players) cards 1 verbose_) strategies
    forM_ players $ \player -> do
      let name = player ^. T.playerName
      putStrLn $ printf "player %s won %d times" name (count name results)

-- player buys a card
buys :: T.PlayerId -> T.Card -> T.Dominion (T.PlayResult ())
buys playerId card = do
    validation <- validateBuy playerId card
    case validation of
      Left x -> return $ Left x
      Right _ -> do
                   money <- handValue playerId
                   modifyPlayer playerId $ \p -> over T.discard (card:) $
                                                 over T.buys (subtract 1) $
                                                 -- this works because extraMoney can be negative
                                                 over T.extraMoney (subtract $ card ^. T.cost) p
                   modify $ over T.cards (delete card)
                   log playerId $ printf "bought a %s" (card ^. T.name)
                   return $ Right ()

-- Give an array of cards, in order of preference of buy.
-- We'll try to buy as many cards as possible, in order of preference.
buysByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
buysByPreference playerId cards = do
    purchasableCards <- filterM (\card -> eitherToBool <$> validateBuy playerId card) cards
    when (not (null purchasableCards)) $ do
      playerId `buys` (head purchasableCards)
      playerId `buysByPreference` cards

-- Give an array of cards, in order of preference of play.
-- We'll try to play as many cards as possible, in order of preference.
playsByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
playsByPreference playerId cards = do
    playableCards <- filterM (\card -> eitherToBool <$> validatePlay playerId card) cards
    when (not (null playableCards)) $ do
      playerId `plays` (head playableCards)
      playerId `playsByPreference` cards

-- player plays an action card. We return an Either.
-- On error, we'll return an error message.
-- On success, we will return either:
--  Nothing, if there are no followup actions to be taken.
--  (playerId, card effect), if that effect has a folow-up
--  action that needs to be taken, such as when throne room
--  is played...you need to then select an action card to
--  play twice.
--
--  You can use the followup with `with`.
plays :: T.PlayerId -> T.Card -> T.Dominion (T.PlayResult (Maybe T.Followup))
playerId `plays` card = do
    validation <- validatePlay playerId card
    case validation of
      Left x -> return $ Left x
      Right _ -> do
               log playerId $ printf "plays a %s!" (card ^. T.name)
               results <- mapM (usesEffect playerId) (card ^. T.effects)
               modifyPlayer playerId (over T.actions (subtract 1))
               if trashThisCard card
                 then playerId `trashesCard` card
                 else playerId `discardsCard` card
               -- we should get at most *one* effect to return
               return . Right . listToMaybe . catMaybes $ results

-- The input of this function is directly the output of `plays`, so you can
-- chain these functions together easily. This automatically handles
-- checking whether the playresult was a Right, and it there is a followup,
-- and applies the followup action if there was something to followup on.
--
-- `with` might lead to more extra effects. And specifically, lets say you
-- play throne room on throne room. Now you have TWO extra effects, i.e.
-- you can choose a card to play twice TWICE.
--
-- Thats why `with` might return an array of extra effects, not just one.
with :: T.Dominion (T.PlayResult (Maybe T.Followup)) -> T.FollowupAction -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
result_ `with` followupAction = do
    result <- result_
    case result of
      Left str -> return $ Left str
      Right Nothing -> return $ Right Nothing
      Right (Just followup) -> followup `_with` followupAction

-- just like `with`, except you can give it an array of followup requests,
-- and an array of followup actions.
withMulti :: T.Dominion (T.PlayResult (Maybe [T.Followup])) -> [T.FollowupAction] -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
results_ `withMulti` followupActions = do
    results <- results_
    case results of
      Left str -> return $ Left str
      Right Nothing -> return $ Right Nothing
      Right (Just followups) -> do
          allResults <- mapM (uncurry _with) (zip followups followupActions)
          return $ Right $ case (concat . catMaybes . rights $ allResults) of
                             [] -> Nothing
                             xs -> Just xs

-- this is what `with` and `withMulti` use behind the scenes. Those
-- functions take care of un-binding the data and extracting it. This is
-- much more simple...this is the core function that takes a followup and
-- a followup action and applies that action.
--
-- of course, if you don't pass in the right followup action, then it will
-- return an error (returns a playresult).
_with :: T.Followup -> T.FollowupAction -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
(playerId, T.PlayActionCard x) `_with` (T.ThroneRoom card) = do
  hasCard <- playerId `has` card
  if not hasCard
    then return . Left $ printf "You don't have a %s in your hand!" (card ^. T.name)
    else do
      log playerId $ printf "playing %s twice!" (card ^. T.name)
      results <- mapM (usesEffect playerId) ((card ^. T.effects) ++ (card ^. T.effects))
      playerId `discardsCard` card
      return $ Right $ case (catMaybes results) of
                 [] -> Nothing
                 xs -> Just xs

(playerId, T.CellarEffect) `_with` (T.Cellar cards) = do
  forM_ cards $ \card -> do
    hasCard <- playerId `has` card
    when hasCard $ do
      playerId `discardsCard` card
      [drawnCard] <- drawFromDeck playerId 1
      log playerId $ printf "discarded a %s and got a %s" (card ^. T.name) (drawnCard ^. T.name)
  return $ Right Nothing

(playerId, T.ChancellorEffect) `_with` (T.Chancellor moveDeck) = do
  when moveDeck $ do
    log playerId "Moving deck into the discard pile"
    modifyPlayer playerId $ \p -> set T.deck [] $ over T.discard (++ (p ^. T.deck)) p
  return $ Right Nothing

(playerId, T.MineEffect) `_with` (T.Mine card) = do
  hasCard <- playerId `has` card
  if not hasCard
    then return . Left $ printf "You don't have a %s in your hand!" (card ^. T.name)
    else if (not . isTreasure $ card)
           then return . Left $ printf "Mine only works with treasure cards, not %s" (card ^. T.name)
           else if (card == CA.gold)
                  then return . Left $ "can't upgrade gold!"
                  else do
                    newCard_ <- getCard (if (card == CA.copper) then CA.silver else CA.gold)
                    case newCard_ of
                      Nothing -> return . Left $ "Sorry, we are out of the card you could've upgraded to."
                      Just newCard -> do
                        playerId `trashesCard` card
                        modifyPlayer playerId $ over T.hand (newCard:)
                        log playerId $ printf "trashed a %s for a %s" (card ^. T.name) (newCard ^. T.name)
                        return $ Right Nothing

(playerId, T.RemodelEffect) `_with` (T.Remodel (toTrash, toGain)) = do
  hasCard <- playerId `has` toTrash
  if not hasCard
    then return . Left $ printf "You don't have a %s in your hand!" (toTrash ^. T.name)
    else if ((toGain ^. T.cost) > (toTrash ^. T.cost) + 2)
           then return . Left $ printf "You're remodeling a %s, a %s is too expensive" (toTrash ^. T.name) (toGain ^. T.name)
           else do
             newCard_ <- getCard toGain
             case newCard_ of
               Nothing -> return . Left $ printf "Sorry, no more %s left" (toGain ^. T.name)
               Just card -> do
                 modifyPlayer playerId $ over T.discard (card:)
                 return $ Right Nothing

(playerId, T.SpyEffect) `_with` (T.Spy (myself, others)) = do
  modifyPlayer playerId (discardTopCard myself)
  modifyOtherPlayers playerId (discardTopCard others)
  return $ Right Nothing

(playerId, T.ThiefEffect) `_with` (T.Thief func) = do
  state <- get
  let players = (indices (state ^. T.players)) \\ [playerId]
  forM_ players $ \pid -> do
    player <- getPlayer pid
    let topCards = take 2 (player ^. T.deck)
        treasures = filter isTreasure topCards
        discards = topCards \\ treasures
    modifyPlayer pid $ over T.deck (drop 2)
    if (null treasures)
      then do
        modifyPlayer pid $ over T.discard (++discards)
        return . Left $ "Sorry, this player had no treasures."
      else do
        let action = func treasures
        case action of
          T.TrashOnly card -> do
            let other = treasures \\ [card]
            modifyPlayer pid $ over T.discard (++other)
            return $ Right Nothing
          T.GainTrashedCard card -> do
            let other = treasures \\ [card]
            modifyPlayer pid $ over T.discard (++other)
            if (card `elem` treasures)
              then do
                modifyPlayer playerId $ over T.discard (card:)
                return $ Right Nothing
              else return $ Left "That card wasn't one of the treasures you could trash!"
  return $ Right Nothing

_ `_with` _ = return $ Left "sorry, you can't play that effect with that extra effect."
