# Dominion

Dominion Simulator. Allows you to test competing strategies and see what works best.

Usage:

    ./dominion [# of iterations -- default 500]

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

You can give `buysByPreference` a list of all the cards you want to buy, in the order you want to buy them in. `buysByPreference` will then buy as many cards as it can.

Here's another example: big money + smithy:

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

It's just like big money, except now the player buys up smithys too. And the player plays a smithy whenever possible (`plays` will play the action card if it can). So now we can compare Big Money vs Big Money + Smithy. And the results are:

Out of 5000 games:

    player adit won 1867 times using "big money"
    player maggie won 3133 times using "big money + smithy"

Just as a control, lets use the same strategy with both players:

    player adit won 2444 times
    player maggie won 2556 times

So clearly, adding the smithy to big money makes a big difference!

### Extra effects

Some action cards have an extra effect. For example, if you use throne room, you can pick another card and play it twice. Here's how that looks:

```haskell
throneRoom playerId = do
    playerId `plays` throneRoom `with` (ThroneRoom market)
```

And of course you can play throne room on throne room:

```haskell
multiThroneRoom playerId = do
    playerId `plays` throneRoom `with` (ThroneRoom throneRoom) `withMulti` [ThroneRoom market, ThroneRoom market]
```

`withMulti` is the same as with, except you specify an array of extra effects you want to do. In this case, we play a throne room on a throne room, so now we can pick an action card to play twice, twice. We choose to play a market both times (assuming we have two markets in our hand).

You can also make sure the player has a market first:

```haskell
throneRoom playerId = do
    player <- getPlayer playerId
    when player `has` market $ do
      playerId `plays` throneRoom `with` (ThroneRoom market)
```
