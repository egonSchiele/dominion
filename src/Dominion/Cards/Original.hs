module Dominion.Cards.Original where
import Dominion.Types
smithy = Card "Smithy" 4 [Action] [PlusDraw 3]
village = Card "Village" 3 [Action] [PlusDraw 1, PlusAction 2]
laboratory = Card "Laboratory" 5 [Action] [PlusDraw 2, PlusAction 1]
festival = Card "Festival" 5 [Action] [PlusAction 2, PlusCoin 2, PlusBuy 1]
market = Card "Market" 5 [Action] [PlusAction 1, PlusCoin 1, PlusDraw 1, PlusBuy 1]
woodcutter = Card "Woodcutter" 3 [Action] [PlusCoin 2, PlusBuy 1]
councilRoom = Card "Council Room" 5 [Action] [PlusDraw 4, PlusBuy 1]
throneRoom = Card "Throne Room" 4 [Action] [PlayActionCard 2]
