# Dominion

An easy to use [Dominion](http://en.wikipedia.org/wiki/Dominion_\(card_game\)) simulator! It allows you to test competing strategies and see what works best.

## Screencast

[![Screenshot](http://static.adit.io/dominion_screencast.png)](https://vimeo.com/84722179)

Usage:

```haskell
import Dominion
import Dominion.Strategies

main = dominion ["adit" `uses` bigMoney, "maggie" `uses` bigMoneySmithy]
```

Or you can pass in options:

```haskell
players = ["adit" `uses` bigMoney, "maggie" `uses` bigMoneySmithy]
main = dominionWithOpts [Log True, Iterations 1] players
```

## Strategies

Here's a simple strategy, the "big money" strategy:

```haskell
bigMoney playerId = playerId `buysByPreference` [province,
                                                 gold,
                                                 duchy,
                                                 silver,
                                                 copper]
```

The big money strategy is simple: buy the most expensive victory or treasure card you can, and repeat.

```haskell
bigMoneySmithy playerId = do
    playerId `plays` smithy
    playerId `buysByPreference` [province,
                                 gold,
                                 duchy,
                                 smithy,
                                 silver,
                                 copper]
```

It's just like big money, except now the player buys up smithys too. And the player plays a smithy whenever possible. So now we can compare Big Money vs Big Money + Smithy. And the results are:

Out of 5000 games:

    player adit won 1867 times using "big money"
    player maggie won 3133 times using "big money + smithy"

Just as a control, lets use the same strategy with both players:

    player adit won 2444 times
    player maggie won 2556 times

So clearly, adding the smithy to big money makes a big difference!

## Followup actions

Some action cards have a followup action. For example, if you use throne room, you can pick another card and play it twice. Here's how that looks:

```haskell
throneRoom playerId = do
    playerId `plays` throneRoom `with` (ThroneRoom market)
```

And of course you can play throne room on throne room:

```haskell
multiThroneRoom playerId = do
    playerId `plays` throneRoom `with` (ThroneRoom throneRoom) `withMulti` [ThroneRoom market,
                                                                            ThroneRoom market]
```

See the full documentation on [Hackage](http://hackage.haskell.org/package/dominion).
