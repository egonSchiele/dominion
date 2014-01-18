import Dominion
import Dominion.Cards
import Dominion.Strategies

main = dominionWithOpts [Iterations 1, Log True] ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
