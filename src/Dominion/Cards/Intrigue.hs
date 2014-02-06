module Dominion.Cards.Intrigue (
  -- | Playing a card is easy:
  --
  -- > playerId `plays` greatHall
  module Dominion.Cards.Intrigue
) where
import Dominion.Types

courtyard = Card "Courtyard" 2 [Action] [PlusCard 3, CourtyardEffect]
pawn = Card "Pawn" 2 [Action] [Choose 2 [PlusCard 1, PlusAction 1, PlusBuy 1, PlusCoin 1]]
secretChamber = Card "Secret Chamber" 2 [Action, Reaction] [SecretChamberEffect]
greatHall = Card "Great Hall" 3 [Victory, Action] [VPValue 1, PlusCard 1, PlusAction 1]
masquerade = Card "Masquerade" 3 [Action] [PlusCard 2, MasqueradeEffect]
shantyTown = Card "Shanty Town" 3 [Action] [PlusAction 2, ShantyTownEffect]
steward = Card "Steward" 3 [Action] [Choose 1 [PlusCard 2, PlusCoin 2, TrashCards 4]]
swindler = Card "Swindler" 3 [Action, Attack] [PlusCoin 2, SwindlerEffect]
wishingWell = Card "Wishing Well" 3 [Action] [PlusCard, PlusAction, WishingWellEffect]
baron = Card "Baron" 4 [Action] [PlusBuy 1, BaronEffect]
bridge = Card "Bridge" 4 [Action] [PlusBuy 1, PlusCoin 1, BridgeEffect]
conspirator = Card "Conspirator" 4 [Action] [PlusCoin 2, ConspiratorEffect]
coppersmith = Card "Coppersmith" 4 [Action] [CoppersmithEffect]
ironworks = Card "Ironworks" 4 [Action] [GainCardUpto 4, IronworksEffect]
miningVillage = Card "Mining Village" 4 [Action] [PlusCard 1, PlusAction 2, MiningVillageEffect]
scout = Card "Scout" 4 [Action] [PlusAction 1, ScoutEffect]
duke = Card "Duke" 5 [Victory] [DukeEffect]
minion = Card "Minion" 5 [Action, Attack] [PlusAction 1, Choose 1 [PlusCoin 2, MinionEffect]]
saboteur = Card "Saboteur" 5 [Action, Attack] [SaboteurEffect]
torturer = Card "Torturer" 5 [Action, Attack] [PlusCard 3, TorturerEffect]
tradingPost = Card "Trading Post" 5 [Action] [TradingPostEffect]
tribute = Card "Tribute" 5 [Action] [TributeEffect]
upgrade = Card "Upgrade" 5 [Action] [PlusCard 1, PlusAction 1, UpgradeEffect]
harem = Card "Harem" 6 [Treasure, Victory] [CoinValue 2, VPValue 2]
nobles = Card "Nobles" 6 [Action, Victory] [VPValue 2, Choose 1 [PlusCard 3, PlusAction 2]]

intrigueCards = [ courtyard,
                  pawn,
                  secretChamber,
                  greatHall,
                  masquerade,
                  shantyTown,
                  steward,
                  swindler,
                  wishingWell,
                  baron,
                  bridge,
                  conspirator,
                  coppersmith,
                  ironworks,
                  miningVillage,
                  scout,
                  duke,
                  minion,
                  saboteur,
                  torturer,
                  tradingPost,
                  tribute,
                  upgrade,
                  harem,
                  nobles
                ]
