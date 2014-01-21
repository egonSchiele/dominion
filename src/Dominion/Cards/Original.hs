module Dominion.Cards.Original (
  -- | Playing a card is easy:
  --
  -- > playerId `plays` adventurer
  module Dominion.Cards.Original                             
) where
import Dominion.Types

adventurer  = Card "Adventurer" 6 [Action] [AdventurerEffect]
bureaucrat  = Card "Bureaucrat" 4 [Action, Attack] [BureaucratEffect]

-- | 
-- > playerId `plays` cellar `with` (Cellar [list of cards to discard])
cellar      = Card "Cellar" 2 [Action] [PlusAction 1, CellarEffect]

-- > To move your deck into the discard pile:
--
-- > playerId `plays` chancellor `with` (Chancellor True)
--
-- If you don't want to, you don't have to use the followup action at all:
--
-- > playerId `plays` chancellor
chancellor  = Card "Chancellor" 3 [Action] [PlusCoin 2, ChancellorEffect]

-- | 
-- > playerId `plays` chapel `with` (Chapel [list of cards to trash])
chapel      = Card "Chapel" 2 [Action] [TrashCards 4]
councilRoom = Card "Council Room" 5 [Action] [PlusCard 4, PlusBuy 1, OthersPlusCard 1]

-- | To gain a market, for example:
--
-- > playerId `plays` feast `with` (Feast market)
feast       = Card "Feast" 4 [Action] [TrashThisCard, GainCardUpto 5]
festival    = Card "Festival" 5 [Action] [PlusAction 2, PlusCoin 2, PlusBuy 1]
laboratory  = Card "Laboratory" 5 [Action] [PlusCard 2, PlusAction 1]
library     = Card "Library" 5 [Action] [LibraryEffect]
market      = Card "Market" 5 [Action] [PlusAction 1, PlusCoin 1, PlusCard 1, PlusBuy 1]
militia     = Card "Militia" 4 [Action, Attack] [PlusCoin 2, OthersDiscardTo 3]

-- | 
-- > playerId `plays` mine `with` (Mine copper)
mine        = Card "Mine" 5 [Action] [MineEffect]
moat        = Card "Moat" 2 [Action, Reaction] [PlusCard 2]
moneylender = Card "Moneylender" 4 [Action] [MoneylenderEffect]

-- | To turn a gold into a province:
--
-- > playerId `plays` remodel `with` (Remodel (gold, province))
remodel     = Card "Remodel" 4 [Action] [RemodelEffect]
smithy      = Card "Smithy" 4 [Action] [PlusCard 3]

-- | The `Spy` `FollowupAction` takes two lists: a list of cards you would
-- discard for yourself, and a list of cards you would discard for others:
--
-- > playerId `plays` spy `with` ([estate, duchy, province], [silver, gold])
spy         = Card "Spy" 4 [Action, Attack] [PlusCard 1, PlusAction 1, SpyEffect]

                    -- gets a list of the treasure cards that the player
                    -- had. You return either TrashOnly to have the player
                    -- trash a card, or GainTrashedCard to gain the trashed
                    -- card.

-- | You need to provide a function that takes a list of treasure cards and
-- picks one to trash. You can either return a `TrashOnly` to trash the
-- card, or a `GainTrashedCard` to put it into your discard pile.
--
-- > playerId `plays` thief `with` (Thief (GainTrashedCard . sortBy (comparing coinValue)))
thief       = Card "Thief" 4 [Action, Attack] [ThiefEffect]

-- | 
-- > playerId `plays` throneRoom `with` (ThroneRoom market)
throneRoom  = Card "Throne Room" 4 [Action] [PlayActionCard 2]
village     = Card "Village" 3 [Action] [PlusCard 1, PlusAction 2]
witch       = Card "Witch" 5 [Action, Attack] [PlusCard 2, OthersGainCurse 1]
woodcutter  = Card "Woodcutter" 3 [Action] [PlusCoin 2, PlusBuy 1]

-- | 
-- > playerId `plays` workshop `with` (Workshop gardens)
workshop    = Card "Workshop" 3 [Action] [GainCardUpto 4]
gardens     = Card "Gardens" 4 [Victory] [GardensEffect]

-- All the action cards from the original game.
originalCards = [adventurer ,
                 bureaucrat ,
                 cellar     ,
                 chancellor ,
                 chapel     ,
                 councilRoom,
                 feast      ,
                 festival   ,
                 laboratory ,
                 library    ,
                 market     ,
                 militia    ,
                 mine       ,
                 moat       ,
                 moneylender,
                 remodel    ,
                 smithy     ,
                 spy        ,
                 thief      ,
                 throneRoom ,
                 village    ,
                 witch      ,
                 woodcutter ,
                 workshop   ,
                 gardens    ]
