module Dominion.Cards.Original where
import Dominion.Types

adventurer  = Card "Adventurer" 6 [Action] [AdventurerEffect]
bureaucrat  = Card "Bureaucrat" 4 [Action, Attack] [BureaucratEffect]
cellar      = Card "Cellar" 2 [Action] [PlusAction 1, CellarEffect]
chancellor  = Card "Chancellor" 3 [Action] [PlusCoin 2, ChancellorEffect]
chapel      = Card "Chapel" 2 [Action] [TrashCards 4]
councilRoom = Card "Council Room" 5 [Action] [PlusCard 4, PlusBuy 1, OthersPlusCard 1]
feast       = Card "Feast" 4 [Action] [TrashThisCard, GainCardUpto 5]
festival    = Card "Festival" 5 [Action] [PlusAction 2, PlusCoin 2, PlusBuy 1]
laboratory  = Card "Laboratory" 5 [Action] [PlusCard 2, PlusAction 1]
library     = Card "Library" 5 [Action] [LibraryEffect]
market      = Card "Market" 5 [Action] [PlusAction 1, PlusCoin 1, PlusCard 1, PlusBuy 1]
militia     = Card "Militia" 4 [Action, Attack] [PlusCoin 2, OthersDiscardTo 3]
mine        = Card "Mine" 5 [Action] [MineEffect]
moat        = Card "Moat" 2 [Action, Reaction] [PlusCard 2]
moneylender = Card "Moneylender" 4 [Action] [MoneylenderEffect]
remodel     = Card "Remodel" 4 [Action] [RemodelEffect]
smithy      = Card "Smithy" 4 [Action] [PlusCard 3]
spy         = Card "Spy" 4 [Action, Attack] [PlusCard 1, PlusAction 1, SpyEffect]
thief       = Card "Thief" 4 [Action, Attack] [ThiefEffect]
throneRoom  = Card "Throne Room" 4 [Action] [PlayActionCard 2]
village     = Card "Village" 3 [Action] [PlusCard 1, PlusAction 2]
witch       = Card "Witch" 5 [Action, Attack] [PlusCard 2, OthersGainCurse 1]
woodcutter  = Card "Woodcutter" 3 [Action] [PlusCoin 2, PlusBuy 1]
workshop    = Card "Workshop" 3 [Action] [GainCardUpto 4]
gardens     = Card "Gardens" 4 [Victory] [GardensEffect]

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
