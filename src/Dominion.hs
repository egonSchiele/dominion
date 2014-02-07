module Dominion (
                -- | How to use: https:\/\/github.com\/egonschiele\/dominion
                module Dominion, 
                Option(..), 
                has, handValue, pileEmpty, getPlayer, cardsOf, validateBuy, validatePlay, getRound, countNum) where

import           Prelude                hiding (log)
import qualified Data.Map.Lazy          as M
import qualified Dominion.Types         as T
import           Dominion.Types         (Option(..))
import qualified Dominion.Cards         as CA
import           Control.Monad          hiding (join)
import           Data.Maybe
import           Control.Monad.State    hiding (state, join)
import           Control.Lens           hiding (indices, has)
import           Control.Monad.IO.Class
import           Text.Printf
import           Data.List
import           Dominion.Utils
import           Data.Either
import           Control.Applicative
import           Dominion.Internal

-- | Convenience function. @ name \`uses\` strategy @ is the same as writing
-- @ (name, strategy) @
uses :: String -> T.Strategy -> (T.Player, T.Strategy)
name `uses` strategy = (makePlayer name, strategy)

-- | The main method to simulate a dominion game. Example:
--
-- > import Dominion
-- > import Dominion.Strategies
-- >
-- > main = dominion ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
dominion :: [(T.Player, T.Strategy)] -> IO [T.Result]
dominion = dominionWithOpts []

-- | Same as `dominion`, but allows you to pass in some options. Example:
--
-- > dominionWithOpts [Iterations 5, Log True] ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
dominionWithOpts :: [T.Option] -> [(T.Player, T.Strategy)] -> IO [T.Result]
dominionWithOpts options list = do
    actionCards_ <- deckShuffle CA.allActionCards
    let actionCards   = take (10 - length requiredCards) actionCards_ ++ requiredCards
        cards         = M.fromList ([(CA.copper, 60), (CA.silver, 40), (CA.gold, 30),
                                    (CA.estate, 12), (CA.duchy, 12), (CA.province, 12)]
                                    ++ [(c, 10) | c <- actionCards])
    when verbose_ $ putStrLn $ "Playing with: " ++ (join ", " . map T._name $ actionCards)
    results <- forM [1..iterations] $ \i -> run (T.GameState (rotate i players) cards 1 verbose_) (rotate i strategies)    
    let winnerNames = map T.winner results
    forM_ players $ \player -> do
      let name = player ^. T.playerName
      putStrLn $ printf "player %s won %d times" name (count name winnerNames)
    return results
  where (players, strategies) = unzip list
        iterations    = fromMaybe 1000 (findIteration options)
        verbose_      = fromMaybe False (findLog options)
        requiredCards = take 10 $ fromMaybe [] (findCards options)
        
-- | Player buys a card. Example:
--
-- > playerId `buys` smithy
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
                   modify $ over T.cards (decrement card)
                   log playerId $ printf "bought a %s" (card ^. T.name)
                   return $ Right ()

-- | Give an array of cards, in order of preference.
-- This function will buy as many cards as possible, in order of
-- preference. For example, suppose you use:
--
-- > playerId `buysByPreference` [province, duchy]
--
-- And you have 16 money and two buys. You will buy two provinces.
-- This runs all the same validations as `buys`.
buysByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
buysByPreference playerId cards = do
    purchasableCards <- filterM (\card -> eitherToBool <$> validateBuy playerId card) cards
    unless (null purchasableCards) $ do
      playerId `buys` head purchasableCards
      playerId `buysByPreference` cards

-- | Give an array of cards, in order of preference.
-- This function will try to play as many cards as possible, in order of preference.
-- Note: if any card requires a `Followup` (like `cellar` or
-- `chapel`), you need to use `plays` instead. This runs all the same
-- validations as `plays`.
playsByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
playsByPreference playerId cards = do
    playableCards <- filterM (\card -> eitherToBool <$> validatePlay playerId card) cards
    unless (null playableCards) $ do
      playerId `plays` head playableCards
      playerId `playsByPreference` cards
 
-- | In the simplest case, this lets you play a card, like this:
--
-- > playerId `plays` smithy
--
-- You can just use this function blindly, without checking to see if you
-- have enough actions, or whether you have a smithy in your hand.
-- `plays` will perform those  validations for you. It returns a `PlayResult`,
-- which is an `Either` with an error message or a return value.  
--
-- Some cards require an additional action. For example, if you use
-- a workshop, you need to specify what card you're going to get. In that
-- case, this function returns a `Followup`. A `Followup` just contains some information about the card you used.
-- You can use the extra action of the card like this:
--
--  > playerId `plays` workshop `with` (Workshop gardens)
--
-- `with` takes a `FollowUp` and a `FollowupAction`, and applies the
-- `FollowupAction`.
-- Here's another example:
--
-- > playerId `plays` throneRoom `with` (ThroneRoom market)
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

-- | You can use `with` to play an FollowupAction. For example:
--
-- > playerId `plays` chapel `with` (Chapel [4 `cardsOf` estate])
--
-- This will trash up to four estates from your hand (depending on how many
-- you have). The input of this function is directly the output of `plays`, so you can
-- chain these functions together easily. This automatically handles
-- checking whether the `PlayResult` was a `Right`, and whether there is
-- a `Followup`, and whether the `FollowupAction` you gave matches the `Followup`,
-- and applies the `FollowupAction`.
--
-- The `FollowupAction` needs to match the `Followup`. You can't do this, for
-- example:
--
-- > playerId `plays` throneRoom `with` (Workshop village)
--
-- You need this instead:
--
-- > playerId `plays` throneRoom `with` (ThroneRoom village)
--
-- `with` might lead to more `Followup`s, in which case you can chain calls
-- using `withMulti`.
with :: T.Dominion (T.PlayResult (Maybe T.Followup)) -> T.FollowupAction -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
result_ `with` followupAction = do
    result <- result_
    case result of
      Left str -> return $ Left str
      Right Nothing -> return $ Right Nothing
      Right (Just followup) -> followup `_with` followupAction

-- | This is just like `with`, except you can give it an array of
-- `Followup`s, and another array of `FollowupAction`s. Most cards will
-- only generate one `Followup`. There's only one case I know about that
-- would generate multiple `Followup`s: playing a throne room on a throne
-- room.
--
-- > playerId `plays` throneRoom `with` (ThroneRoom throneRoom) `withMulti` [ThroneRoom market, ThroneRoom smithy]
--
-- Here, someone plays a throne room on a throne room. Now you have to
-- follow up with two action cards: the two cards you want to play twice.
-- The player passes in `market` and `smithy`, and they both get played twice.
withMulti :: T.Dominion (T.PlayResult (Maybe [T.Followup])) -> [T.FollowupAction] -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
results_ `withMulti` followupActions = do
    results <- results_
    case results of
      Left str -> return $ Left str
      Right Nothing -> return $ Right Nothing
      Right (Just followups) -> do
          allResults <- zipWithM _with followups followupActions
          return $ Right $ case concat . catMaybes . rights $ allResults of
                             [] -> Nothing
                             xs -> Just xs

-- | `with` and `withMulti` automatically extract the `Followup` out of the
-- result of `plays`. If you have a `Followup` already, or you want more
-- control, you can use this instead.
--
-- > result <- playerId `plays` throneRoom
-- > case result of
-- >   Left str -> return . Left $ str
-- >   Right followup -> followup `_with` (ThroneRoom market)
_with :: T.Followup -> T.FollowupAction -> T.Dominion (T.PlayResult (Maybe [T.Followup]))
(playerId, T.PlayActionCard x) `_with` (T.ThroneRoom card) = do
  hasCard <- playerId `has` card
  if not hasCard
    then return . Left $ printf "You don't have a %s in your hand!" (card ^. T.name)
    else do
      log playerId $ printf "playing %s twice!" (card ^. T.name)
      results <- mapM (usesEffect playerId) ((card ^. T.effects) ++ (card ^. T.effects))
      playerId `discardsCard` card
      return $ Right $ case catMaybes results of
                 [] -> Nothing
                 xs -> Just xs

(playerId, T.CellarEffect) `_with` (T.Cellar cards) = do
  hand <- currentHand playerId
  let ownedCards = hand `intersect` cards
      numCards = length ownedCards
  forM_ ownedCards $ \card -> do
    playerId `discardsCard` card
    log playerId $ printf "discarded a %s" (card ^. T.name)
  drawFromDeck playerId numCards
  log playerId $ printf "drew %d cards" numCards
  return $ Right Nothing

(playerId, T.ChancellorEffect) `_with` (T.Chancellor moveDeck) = do
  when moveDeck $ do
    log playerId "Moving deck into the discard pile"
    modifyPlayer playerId $ \p -> set T.deck [] $ over T.discard (++ (p ^. T.deck)) p
  return $ Right Nothing

(playerId, T.TrashCards x) `_with` (T.Chapel cards) = do
  let toTrash = take x cards
  forM_ toTrash $ \card_ -> playerId `trashesCard` card_
  return $ Right Nothing

(playerId, T.GainCardUpto x) `_with` (T.Feast card) = gainCardUpTo playerId x card
(playerId, T.GainCardUpto x) `_with` (T.Workshop card) = gainCardUpTo playerId x card


(playerId, T.MineEffect) `_with` (T.Mine card) = do
  hasCard <- playerId `has` card
  let check = do
        failIf (not hasCard) $ printf "You don't have a %s in your hand!" (card ^. T.name)
        failIf (not . isTreasure $ card) $ printf "Mine only works with treasure cards, not %s" (card ^. T.name)
        failIf (card == CA.gold) "can't upgrade gold!"
  case check of
    Left str -> return $ Left str
    Right _ -> do
      newCard_ <- getCard $ if card == CA.copper then CA.silver else CA.gold
      case newCard_ of
        Nothing -> return . Left $ "Sorry, we are out of the card you could've upgraded to."
        Just newCard -> do
          playerId `trashesCard` card
          modifyPlayer playerId $ over T.hand (newCard:)
          log playerId $ printf "trashed a %s for a %s" (card ^. T.name) (newCard ^. T.name)
          return $ Right Nothing

(playerId, T.RemodelEffect) `_with` (T.Remodel (toTrash, toGain)) = do
  hasCard <- playerId `has` toTrash
  let check = do
        failIf (not hasCard) $ printf "You don't have a %s in your hand!" (toTrash ^. T.name)
        let tooExpensive = (toGain ^. T.cost) > (toTrash ^. T.cost) + 2
        failIf tooExpensive $ printf "You're remodeling a %s, a %s is too expensive" (toTrash ^. T.name) (toGain ^. T.name)
  case check of
    Left str -> return $ Left str
    Right _ -> do
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
  let players = indices (state ^. T.players) \\ [playerId]
  forM_ players $ \pid -> do
    player <- getPlayer pid
    let topCards = take 2 (player ^. T.deck)
        treasures = filter isTreasure topCards
        discards = topCards \\ treasures
    modifyPlayer pid $ over T.deck (drop 2)
    if null treasures
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
            if card `elem` treasures
              then do
                modifyPlayer playerId $ over T.discard (card:)
                return $ Right Nothing
              else return $ Left "That card wasn't one of the treasures you could trash!"
  return $ Right Nothing

_ `_with` _ = return $ Left "sorry, you can't play that effect with that extra effect."
